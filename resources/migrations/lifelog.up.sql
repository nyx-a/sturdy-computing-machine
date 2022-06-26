create table lifelog (
  id   integer primary key autoincrement,
  date datetime,
  text text,
  unique (date, text)
);
