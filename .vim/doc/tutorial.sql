
    CREATE TABLE customer(
        id              INTEGER NOT NULL,
        cust_name       VARCHAR(30) NOT NULL,
        phone_nbr       VARCHAR(30) NULL,
        PRIMARY KEY(id)
    );

    INSERT INTO customer(id, cust_name, phone_nbr)
    VALUES( 1, 'Bob', '555-1210' );
    INSERT INTO customer(id, cust_name, phone_nbr)
    VALUES( 2, 'Jim', '555-1211' );
    INSERT INTO customer(id, cust_name, phone_nbr)
    VALUES( 3, 'Ted', '555-1212' );
    INSERT INTO customer(id, cust_name, phone_nbr)
    VALUES( 4, 'Sid', '555-1213' );
    INSERT INTO customer(id, cust_name, phone_nbr)
    VALUES( 5, 'Joe', '555-1214' );
    COMMIT;

    CREATE TABLE contact(
        id              INTEGER NOT NULL,
        cont_name       VARCHAR(30) NOT NULL,
        phone_nbr       VARCHAR(30) NULL,
        PRIMARY KEY(id)
    );

    INSERT INTO contact(id, cont_name, phone_nbr)
    VALUES( 10, 'Bob', '555-1210' );
    INSERT INTO contact(id, cont_name, phone_nbr)
    VALUES( 20, 'Jim', '555-1211' );
    INSERT INTO contact(id, cont_name, phone_nbr)
    VALUES( 30, 'Ted', '555-1212' );
    INSERT INTO contact(id, cont_name, phone_nbr)
    VALUES( 40, 'Sid', '555-1213' );
    INSERT INTO contact(id, cont_name, phone_nbr)
    VALUES( 50, 'Joe', '555-1214' );
    COMMIT;

    SELECT id FROM customer;

    SELECT id FROM different_owner.customer;

    SELECT id 
      INTO @cust_variable_id
      FROM customer
     WHERE cust_name LIKE :host_var_name
       AND cust_name LIKE ?
       AND id        =    @var_name;
