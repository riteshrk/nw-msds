/* 7. Write and execute the SQL command to list the number of product sales
(number of rows) and total sales by month, product category, and product. 
Your output should be sorted by month, product category and product. */
SELECT tm_month, p_category, p_descript, COUNT(sale_units) AS num_sales, 
SUM(sale_units*sale_price) AS total_sales
FROM dwdaysalesfact
JOIN dwtime ON dwdaysalesfact.tm_id = dwtime.tm_id
JOIN dwproduct ON dwdaysalesfact.p_code = dwproduct.p_code
GROUP BY tm_month, p_category, p_descript
ORDER BY tm_month, p_category, p_descript;