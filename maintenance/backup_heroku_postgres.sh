heroku addons:add pgbackups # To install the addon
curl -o latest.dump `heroku pgbackups:url` # To download a dump
pg_restore --data-only --file=out.sql latest.dump
psql -f ./out.sql
