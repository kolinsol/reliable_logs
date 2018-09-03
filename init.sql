create table logs(
    request_id serial primary key,
    log_created timestamp not null,
    created timestamp not null default now(),
    app_id text not null,
    object_id bigint not null,
    tags jsonb not null,
    message text not null,
    context jsonb
);

create index app_id_idx on logs (app_id);
create index object_id_idx on logs (object_id);
create index log_created on logs (log_created);
