-module(cmconfig).
-export([
         apps/0,
         app/1,
         buckets/0,
         bucket/1,
         crons/0,
         cron/1,
         modules/0,
         module/1,
         ports/0,
         port/1,
         queues/0,
         queue/1,
         settings/0,
         settings/1,
         tasks/0,
         task/1,
         templates/0,
         template/1,
         tests/0,
         test/1,
         themes/0,
         theme/1,
         topics/0,
         topic/1,
         effects/0,
         effect/1,
         metrics/0,
         metrics/1
        ]).

apps() -> {ok, V} = cmkit:app_env(cmconfig, app), V.
app(N) -> cmkit:app_env(cmconfig, app, N).
buckets() -> {ok, V} = cmkit:app_env(cmconfig, bucket), V.
bucket(N) -> cmkit:app_env(cmconfig, bucket, N).
crons() -> {ok, V} = cmkit:app_env(cmconfig, cron), V.
cron(N) -> cmkit:app_env(cmconfig, cron, N).
modules() -> {ok, V} = cmkit:app_env(cmconfig, module), V.
module(N) -> cmkit:app_env(cmconfig, module, N).
ports() -> {ok, V} = cmkit:app_env(cmconfig, port), V.
port(N) -> cmkit:app_env(cmconfig, port, N).
queues() -> {ok, V} = cmkit:app_env(cmconfig, queue), V.
queue(N) -> cmkit:app_env(cmconfig, queue, N).
settings() -> {ok, V} = cmkit:app_env(cmconfig, settings), V.
settings(N) -> cmkit:app_env(cmconfig, settings, N).
tasks() -> {ok, V} = cmkit:app_env(cmconfig, task), V.
task(N) -> cmkit:app_env(cmconfig, task, N).
templates() -> {ok, V} = cmkit:app_env(cmconfig, template), V.
template(N) -> cmkit:app_env(cmconfig, template, N).
tests() -> {ok, V} = cmkit:app_env(cmconfig, test), V.
test(N) -> cmkit:app_env(cmconfig, test, N).
themes() -> {ok, V} = cmkit:app_env(cmconfig, theme), V.
theme(N) -> cmkit:app_env(cmconfig, theme, N).
topics() -> {ok, V} = cmkit:app_env(cmconfig, topic), V.
topic(N) -> cmkit:app_env(cmconfig, topic, N).
metrics() -> {ok, V} = cmkit:app_env(cmconfig, metrics), V.
metrics(N) -> cmkit:app_env(cmconfig, metrics, N).

effects() -> cmeffect:effects().

effect(N) ->
  cmeffect:effect(N).
