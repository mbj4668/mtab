# Mtab - An Erlang library for pretty-printing tabular data

Prints lists of data as pretty tables, in various styles.

## Features

- Data can be lists of tuples, lists, proplists and maps
- Handles sparse tables
- Handles multi-line data
- Configure the column print order
- Built in styles
- Custom styles

## Examples

The following examples shows how a simple table looks in the different
builtin styles.

The data is defined as:

```
Data = [{"Year","Album","Songs"},
        {1981,"Kollaps",13},
        {1983,"Zeichnungen Des Patienten O.T.",12},
        {1993,"Tabula rasa",7},
        {2004,"Perpetuum Mobile",12}]
```

### plain

```
mtab:format(Data, #{style => plain})

Year Album                          Songs
1981 Kollaps                        13
1983 Zeichnungen Des Patienten O.T. 12
1993 Tabula rasa                    7
2004 Perpetuum Mobile               12
```


### simple

```
mtab:format(Data, #{style => simple})

Year Album                          Songs
---- ------------------------------ -----
1981 Kollaps                        13
1983 Zeichnungen Des Patienten O.T. 12
1993 Tabula rasa                    7
2004 Perpetuum Mobile               12
```


### pretty

```
mtab:format(Data, #{style => pretty})

+------+--------------------------------+-------+
| Year | Album                          | Songs |
+------+--------------------------------+-------+
| 1981 | Kollaps                        | 13    |
| 1983 | Zeichnungen Des Patienten O.T. | 12    |
| 1993 | Tabula rasa                    | 7     |
| 2004 | Perpetuum Mobile               | 12    |
+------+--------------------------------+-------+
```


### simple_pretty

```
mtab:format(Data, #{style => simple_pretty})

| Year | Album                          | Songs |
+------+--------------------------------+-------+
| 1981 | Kollaps                        | 13    |
| 1983 | Zeichnungen Des Patienten O.T. | 12    |
| 1993 | Tabula rasa                    | 7     |
| 2004 | Perpetuum Mobile               | 12    |
```


### presto

```
mtab:format(Data, #{style => presto})

 Year | Album                          | Songs
------+--------------------------------+-------
 1981 | Kollaps                        | 13
 1983 | Zeichnungen Des Patienten O.T. | 12
 1993 | Tabula rasa                    | 7
 2004 | Perpetuum Mobile               | 12
```


### ascii

```
mtab:format(Data, #{style => ascii})

+------+--------------------------------+-------+
| Year | Album                          | Songs |
+------+--------------------------------+-------+
| 1981 | Kollaps                        | 13    |
+------+--------------------------------+-------+
| 1983 | Zeichnungen Des Patienten O.T. | 12    |
+------+--------------------------------+-------+
| 1993 | Tabula rasa                    | 7     |
+------+--------------------------------+-------+
| 2004 | Perpetuum Mobile               | 12    |
+------+--------------------------------+-------+
```


### grid

```
mtab:format(Data, #{style => grid})

+------+--------------------------------+-------+
| Year | Album                          | Songs |
+======+================================+=======+
| 1981 | Kollaps                        | 13    |
+------+--------------------------------+-------+
| 1983 | Zeichnungen Des Patienten O.T. | 12    |
+------+--------------------------------+-------+
| 1993 | Tabula rasa                    | 7     |
+------+--------------------------------+-------+
| 2004 | Perpetuum Mobile               | 12    |
+------+--------------------------------+-------+
```


### simple_grid

```
mtab:format(Data, #{style => simple_grid})

| Year | Album                          | Songs |
+======+================================+=======+
| 1981 | Kollaps                        | 13    |
+------+--------------------------------+-------+
| 1983 | Zeichnungen Des Patienten O.T. | 12    |
+------+--------------------------------+-------+
| 1993 | Tabula rasa                    | 7     |
+------+--------------------------------+-------+
| 2004 | Perpetuum Mobile               | 12    |
```


## Data as maps

The following examples shows data as a list of maps, how to control
column order, and how to control cell formatting.

```
Data = [#{album => "Kollaps",songs => 13,year => 1981},
        #{album => "Zeichnungen Des Patienten O.T.",songs => 12,year => 1983},
        #{album => "Tabula rasa",songs => 7,year => 1993},
        #{album => "Perpetuum Mobile",songs => 12,year => 2004}]
```

When the data is a map, the header is by default the map keys of the
first row.

```
mtab:format(Data, #{style => presto,header_fmt => titlecase})

 Album                          | Songs | Year
--------------------------------+-------+------
 Kollaps                        | 13    | 1981
 Zeichnungen Des Patienten O.T. | 12    | 1983
 Tabula rasa                    | 7     | 1993
 Perpetuum Mobile               | 12    | 2004
```


```
mtab:format(Data, #{header => [year,album,songs],
                    cols =>
                        [#{width => 6,align => left},
                         #{align => center},
                         #{align => right}],
                    style => presto,header_fmt => titlecase})

 Year   |             Album              | Songs
--------+--------------------------------+-------
 1981   |            Kollaps             |    13
 1983   | Zeichnungen Des Patienten O.T. |    12
 1993   |          Tabula rasa           |     7
 2004   |        Perpetuum Mobile        |    12
```
