# Security Policy

## Supported Versions

Only the latest stable release receives security patches.

## Reporting a Vulnerability

Please **do not** open a public GitHub issue for security
vulnerabilities.

Use [GitHub’s private vulnerability
reporting](https://github.com/krose/entsoeapi/security/advisories/new)
to report issues confidentially.

We aim to acknowledge reports within **7 days** and release a patch
within **90 days**, depending on severity.

## Scope

In scope: - Leakage or improper handling of the `ENTSOE_PAT` API token -
Unsafe processing of API responses (XML/JSON parsing) - Vulnerable
transitive dependencies

Out of scope: - Vulnerabilities in the ENTSO-E API itself - Issues
requiring a compromised R environment
