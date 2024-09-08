-- 1. Write a query to count the number of invoices.
SELECT COUNT(*) AS invoice_count FROM invoice;


-- 2. Write a query to count the number of customers with a customer balance over $500.
SELECT COUNT(*) FROM customer WHERE cus_balance > 500;

-- 3. Generate a listing of all purchases made by the customers
SELECT
    c.cus_code,
    c.cus_lname,
    c.cus_fname,
    i.inv_number,
    i.inv_date,
    l.p_code,
    p.p_descript
FROM
    public.customer c
JOIN
    public.invoice i ON c.cus_code = i.cus_code
JOIN
    public.line l ON i.inv_number = l.inv_number
JOIN
    public.product p ON l.p_code = p.p_code
ORDER BY
    c.cus_code, i.inv_number;

-- 4. Generate the listing of customer purchases, including the subtotals for each of the invoice line numbers

SELECT
    c.cus_code,
    c.cus_lname,
    c.cus_fname,
    i.inv_number,
    i.inv_date,
    l.p_code,
    pr.p_descript,
    (l.line_units * l.line_price) AS line_subtotal
FROM
    public.customer c
JOIN
    public.invoice i ON c.cus_code = i.cus_code
JOIN
    public.line l ON i.inv_number = l.inv_number
JOIN
    public.p pr ON l.p_code = pr.p_code  -- Join the product table to include product descriptions
ORDER BY
    i.inv_number, l.line_number;


-- 5. List the balance characteristics of the customers who have made purchases during the current 
-- invoice cycle—that is, for the customers who appear in the INVOICE table.

SELECT
    c.cus_code,
    c.cus_lname,
    c.cus_fname,
    c.cus_balance
FROM
    public.customer c
WHERE
    c.cus_code IN (SELECT DISTINCT inv.cus_code FROM public.invoice inv)
ORDER BY
    c.cus_code;
	
	

-- 6. Find the listing of customers who did not make purchases during the invoicing period.

SELECT
    c.cus_code,
    c.cus_lname,
    c.cus_fname
FROM
    public.customer c
LEFT JOIN
    public.invoice i ON c.cus_code = i.cus_code
WHERE
    i.cus_code IS NULL
ORDER BY
    c.cus_code;
	
	
-- 7. Create a query to produce the summary of the value of products currently in inventory.
SELECT SUM(p_qoh * p_price) AS total_inventory_value
FROM public.p;


