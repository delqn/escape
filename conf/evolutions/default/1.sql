DROP TABLE if exists Users;

CREATE TABLE Users (
    id BIGSERIAL,
    email varchar(255) NOT NULL,
    fullname varchar(255),
    facebookusername varchar(255),
    facebookid bigint,
    PRIMARY KEY (id)
);
