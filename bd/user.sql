CREATE TABLE User (
    firstname VARCHAR(30),
    lastname VARCHAR(30),
    email VARCHAR(30) NOT NULL,
    password VARCHAR(30) NOT NULL,
    country VARCHAR(30) NOT NULL,
    city VARCHAR(30) NOT NULL,
    zipcode VARCHAR(6),
    PRIMARY KEY (email)
);

