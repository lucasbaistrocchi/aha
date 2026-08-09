# Deploying the AMS (free, password-protected)

Target: **Posit Connect Cloud** — free, deploys from a public GitHub repo, and
is where Posit is steering shinyapps.io users. The app is gated by a shared
staff password, and the availability board saves to a Google Sheet so nothing
is lost when the server restarts.

Total time: about 45 minutes, most of it the one-off Google setup in Step 2.

---

## Step 1 — Put the code on GitHub

1. Create a GitHub account, then a **new repository** (public is required for
   the Connect Cloud free tier — that's fine, the *code* is public, your
   *data* is not: the athlete data lives in Google Sheets and behind the
   password gate).
2. Upload the whole `rugby-ams` folder. The included `.gitignore` keeps
   credentials and local CSVs out.

> **Check before pushing:** no `.json` key file, no `.Renviron`. If
> `git status` ever lists a file ending in `.json` other than
> `manifest.json`, stop and remove it.

## Step 2 — Create a Google service account (one time)

The app must write the availability board without a human clicking an OAuth
prompt. A *service account* is a robot Google identity that does exactly that.

1. Go to <https://console.cloud.google.com/> → create a project (any name).
2. **APIs & Services → Library** → enable **Google Sheets API**.
3. **APIs & Services → Credentials → Create credentials → Service account**.
   Name it `rugby-ams`, click through, and create it.
4. Open the new service account → **Keys → Add key → Create new key → JSON**.
   A `.json` file downloads. **This is a password — never commit it.**
5. Open the JSON and copy the `client_email` value. It looks like
   `rugby-ams@your-project.iam.gserviceaccount.com`.
6. Open your **existing availability sheet** (the one with athletes down the
   side and session dates across the top). Click **Share**, paste that
   `client_email`, and give it **Editor** access.

That's the only sheet the app writes to. The GPS, wellness, and testing
sheets are read-only and need no sharing — link-shared is enough.

## Step 3 — Generate `manifest.json`

Connect Cloud needs a manifest listing the app's R packages. Run this **once**
in RStudio from inside the `rugby-ams` folder, then commit the result:

```r
install.packages("rsconnect")
rsconnect::writeManifest()
```

Re-run it any time you add a new `library()` call.

## Step 4 — Deploy

1. Sign up at <https://connect.posit.cloud/> with your GitHub account.
2. **Publish → Shiny → R**, pick the repo, set the primary file to `app.R`.
3. Before the first launch, add these **environment variables / secrets**:

| Variable | Value |
|---|---|
| `APP_PASSWORD` | the shared staff password you choose |
| `GS4_SERVICE_ACCOUNT_JSON` | paste the **entire contents** of the JSON key file |

(The availability sheet ID is already built in; set `AVAILABILITY_SHEET_ID`
only if you switch to a different sheet.)

4. Publish. First boot takes several minutes while packages install.

## Step 5 — Verify

Open the URL and confirm:

- [ ] Login screen appears; wrong password rejected; no red "unprotected" banner
- [ ] GPS / Wellness / Testing badges read **LIVE** (not DEMO)
- [ ] Availability tab says *"Edits save to the availability sheet"*
      (amber "Read-only" means the service account isn't working)
- [ ] Availability shows the statuses already in your sheet for 8/10
- [ ] Change one athlete to "Injured" — the cell updates in the sheet

Share the URL and password with staff. Change the password by editing
`APP_PASSWORD` and restarting.

---

## Things to know

**Free tier = public URL.** The password gate is genuine protection (nothing
renders until you sign in), but it's a single shared password, not per-user
accounts. Treat the URL as sensitive, don't post it publicly, and change the
password when staff leave.

**Sleeping.** Free apps sleep when idle and take ~30 seconds to wake. Normal.

**Usage limits.** If the app becomes unavailable mid-month, you've hit the
free plan's limits — the fix is a paid tier or self-hosting on a facility
machine.

**Updating data.** Nothing to redeploy: GPS, wellness, and testing all read
from Google Sheets on load and refresh every 15 minutes.

**Updating targets.** The pre-season forecasts and match benchmarks come from
`data/2026-2027 LIFE U GPS Master Database.xlsx`. Changing those means
committing an updated workbook and redeploying.

**Without the Google setup**, the app still runs — it just saves availability
to a local file, which a hosted container wipes on restart. The Availability
tab warns you in amber when it's in that state.
