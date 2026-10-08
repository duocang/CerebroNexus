# Linux / Shiny Server / Docker authentication deployment

This note ships with generated Apps at `viewer/AUTHENTICATION-DEPLOYMENT.md`.
Apply the checks to the **final deployed App**, after copying or extracting it.

## Authentication sources

`CEREBRO_AUTH_PASSPHRASE` is the key for the encrypted authentication database,
**not a user's login password**. Keep `viewer-auth.env` and
`private-data/auth/credentials.sqlite` from the same build together. Do not
generate a replacement key for an existing database.

The application uses a nonempty process environment variable first. When it
is set, the local `viewer-auth.env` is not read. Otherwise, Builder Apps load
that file from their own App directory. A shell export is not automatically
inherited by an already running Shiny Server, systemd service or container;
the key must reach the actual R application process through the deployment's
secret/environment configuration. Do not put the key in logs, a Dockerfile,
version control or a command that prints it.

The local file must contain exactly one line, without quotes or `export`:

```text
CEREBRO_AUTH_PASSPHRASE=<64 lowercase hexadecimal characters>
```

The angle-bracket text above is a description, not a key to copy. Restore the
original generated file if its content is damaged. Scripted `createShinyApp()`
exports using an externally supplied key must provide the configured process
environment variable; that API does not generate a local secret file.

## Ownership and permissions

Recommended final Linux permissions:

| Path relative to the App directory | Mode |
| --- | --- |
| `viewer-auth.env` | `0600` (required for local-file loading) |
| `private-data/auth/credentials.sqlite` | `0600` |
| `private-data/auth` | `0700` |

The actual Shiny/R application user must be able to read both files and
traverse every parent directory. With these modes, set ownership to that
user. Do not assume a fixed UID/GID, or use the Shiny Server master process's
root identity in place of the R worker's identity. Check `run_as` in Shiny
Server's configuration and inspect the R processes, for example:

```bash
ps -eo user,uid,gid,comm
```

From the deployed App directory, replace the placeholders with the verified
runtime IDs before running:

```bash
sudo chown <shiny_uid>:<shiny_gid> viewer-auth.env \
  private-data/auth private-data/auth/credentials.sqlite
sudo chmod 600 viewer-auth.env private-data/auth/credentials.sqlite
sudo chmod 700 private-data/auth
stat -c '%a %u:%g %n' viewer-auth.env \
  private-data/auth private-data/auth/credentials.sqlite
```

Then restart the App using the deployment's normal procedure and verify
startup and login as the application user. If using only an externally
configured key, omit the local-file commands. Read-only database access is
supported; do not make the database or mount writable merely to start the App.

## Docker bind mounts, including read-only mounts

For a layout such as `./apps:/srv/shiny-server/shiny:ro`, the final files come
from the host. A Dockerfile's `RUN chmod` or `RUN chown` on the image directory
does not fix files that a later bind mount places over that directory.

1. Identify the R worker UID/GID **inside the running container**. Replace the
   service placeholder with your actual Compose service name:

   ```bash
   docker compose exec <service> ps -eo user,uid,gid,comm
   ```

2. Apply the ownership/mode commands above **on the host's exported App
   directory**, such as `./apps/<app>`. For rootless Docker or user-namespace
   remapping, account for the host-to-container UID/GID mapping; do not assume
   the same numeric IDs on both sides. Verify the resulting mode and owner as
   seen from the container.
3. Restart the App/container with your deployment's normal procedure, then
   confirm startup and login.

Do not attempt `chmod` or `chown` from inside a read-only bind mount. Keep the
mount read-only. The App reports problems but never changes deployment file
permissions or ownership automatically.

## Windows builds and transfers

Windows does not provide the same POSIX permission semantics. A successful
Windows build does **not** certify Linux `0600`/`0700` modes. Copies, ZIP
extraction, upload tools and bind mounts can change permissions or ownership.
Apply and verify the final settings on the Linux host after transfer, even
when Linux-to-Linux tools claim to preserve permissions.

## Startup errors

- **Not found / could not be inspected:** check the App directory, missing
  files and traversal permissions. Restore the matching file or provide the
  configured process environment variable.
- **Insecure permissions (for example `0664` or `0644`):** local-file loading
  requires exactly `0600`; correct the final file's mode and ownership.
- **Secure permissions but not readable:** `chmod 600` alone is insufficient
  when the file belongs to another user. Check the R worker's UID/GID,
  ownership and parent directories.
- **Symbolic link / not a regular file:** the local-file security check
  rejects it. Deploy the real file or use the process environment source.
- **Invalid content:** restore the original matching file; do not insert a
  login password or a new random database key.
- **Database not accessible:** check both `private-data/auth` and
  `credentials.sqlite` ownership and access.
- **Database or passphrase invalid:** ensure the database and key match. A
  stale process environment variable takes precedence over a correct local
  file.
