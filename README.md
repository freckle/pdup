# PDUp

Report outage minutes and calculate uptime based on PagerDuty incidents.

## Features

- Handles Retry (on 429) and pagination of PagerDuty API
- Accepts days or absolute time range (see usage)
- Combines overlapping Incidents (in other words, you can only be down 1 minute
  in any given minute)

## Installation

```
git clone https://github.com/freckle/pdup
cd pdup
stack build --fast --pedantic
```

Create `.env`:

```
PAGERDUTY_TOKEN={Your PagerDuty API token}
```

## Usage

```console
% stack exec pdup -- --help
Usage: pdup ((-d|--days N) | (-s|--since YYYY-MM-DD) [-u|--until YYYY-MM-DD])

Available options:
  -d,--days N              Query over previus N days from now
  -s,--since YYYY-MM-DD    Query since start of date
  -u,--until YYYY-MM-DD    Query until end of
                           date (default: 2020-04-15 19:18:58.33700956 UTC)
  -h,--help                Show this help text
```

To show information for the previous 7, 30, and 90 days, run `bin/report`.

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)
