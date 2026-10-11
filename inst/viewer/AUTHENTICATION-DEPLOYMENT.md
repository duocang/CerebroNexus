# Authentication: offline deployment checklist

Full setup, Shiny Server/systemd commands, rotation and troubleshooting:
[Authentication guide](https://mihem.github.io/CerebroNexus/articles/control_access_to_cerebro_with_a_login_page.html).

- Keep `private-data/auth/credentials.sqlite` paired with its original database
  passphrase. The passphrase is not a user's login password.
- Supply the configured passphrase environment variable to the actual R worker,
  not only your interactive shell. A nonempty process variable takes precedence
  over a generated App's local `viewer-auth.env`.
- If a generated App includes `viewer-auth.env`, preserve it privately. Its
  generated format is `CEREBRO_AUTH_PASSPHRASE=<64 lowercase hexadecimal characters>`
  on one line, without quotes or `export`. Restore the original if damaged.
  Scripted exports using an external key do not generate this file.
- On Linux the local secret requires mode `0600`. Recommended modes are `0600`
  for the database and `0700` for its directory. The actual Shiny/R runtime user
  must own or have appropriate access and traverse every parent directory.
- Verify permissions after copying, especially from Windows to Linux. A Windows
  startup does not test POSIX ownership. For read-only storage, correct source
  permissions before deployment; the App does not change them automatically.
- Keep secrets out of source control and logs. Test valid/invalid login on the
  deployed host, and use HTTPS for access beyond the local machine.
