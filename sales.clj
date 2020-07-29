(ns sales (:require [clojure.test :refer :all]
                  [clojure.string :as str]))
;;======================================================================================================================
;;Utility Function to split lines using "|" as delimiter
(defn splitUsingDelimiter [line]
  (str/split line #"\|"))
;======================================================================================================================
;;CHOICE 1
;;The fetchCustomerData function takes in cust.txt as input and reduces the string(using slurp) into a hashmap using a
;;apply function. Also, we parse the ID to the Integer values for further uses.
(defn fetchCustomerData[]
  (def allCustomersRawData(slurp "C:\\Users\\jenis\\Desktop\\cust.txt"))
  (def allCustomerRawDataLines (str/split allCustomersRawData  #"\n"))
  (def personalData (map(fn[eachLine] (splitUsingDelimiter eachLine)) allCustomerRawDataLines))
  (def dividedDataPairs (into(sorted-map)(apply merge( map #(hash-map (Integer/parseInt (str/trim (first %1))) (vec (rest %1))) personalData)))))
;======================================================================================================================
;;CHOICE 2
;;The fetchProductData function takes in prod.txt as input and reduces the string(using slurp) into a hashmap using a
;;apply function.  Also, we parse the ID to the Integer values for further uses.
(defn fetchProductData[]
  (def allProducts(slurp "C:\\Users\\jenis\\Desktop\\prod.txt"))
  (def eachProduct (str/split allProducts  #"\n"))
  (def eachProductData (map(fn[eachLine] (splitUsingDelimiter eachLine)) eachProduct))
  (def processedProduct (into(sorted-map)(apply merge( map #(hash-map  (Integer/parseInt (str/trim (first %1))) (vec (rest %1))) eachProductData)))))
;======================================================================================================================
;CHOICE 3
;;The fetchSalesData function takes in sales.txt as input and reduces the string(using slurp) into a hashmap using a
;;apply function.  Also, we parse the ID to the Integer values for further uses.
(defn fetchSalesData[]
  (def allSalesRawData(slurp "C:\\Users\\jenis\\Desktop\\sales.txt"))
  (def allSalesRawLines (str/split allSalesRawData  #"\n"))
  (def eachSaleData (map(fn[eachLine] (splitUsingDelimiter eachLine)) allSalesRawLines))
  (def processedSaleData (into(sorted-map)(apply merge( map #(hash-map  (Integer/parseInt (str/trim (first %1)))   (vec (rest %1))) eachSaleData)))))
;======================================================================================================================
;The findreplacements function is used to find the name of the customer and the product using the ID in the sales Table.
;To do so, we loop through the customer as well as processedProducts hashmap and store the ID in the variable when the customer ID 
;and product ID matches the sales table customer ID.
(defn findReplacements[customerToFind productToFind]
  (def resultCustomer "NONE!!")
  (doseq [[customerID valuePairs] dividedDataPairs]
    (def keyRound (str customerID))
    (def custID (str customerToFind))
    (if(= keyRound custID)
      (def resultCustomer (get valuePairs 0))))
  (def resultProduct "NONE!!")
  (doseq [[productID valuePairProducts] processedProduct]
    (def prodKey (str productID))
    (def prodId (str productToFind))
    (if(= prodKey prodId)
      (def resultProduct (get valuePairProducts 0)))))
;;========================================================================================================================
;CHOICE 4
; the getTotalAmount funciton is used to calculate the total expense for a customer NAME input. EX. Sue Jones: $ 20.90
;First using the input name, the customer ID is being fetched and then, using that ID, the corresponding product ID is
;fetched which is then multiplied to the price of that product and then added to the total sum, which is then done for all products.
(defn getTotalAmount [nameToLook]
  (def idToLook 0)
  (def totalAmount 0.0)
  (doseq [[customerID valuePairs] dividedDataPairs]
    (def compareString (str/trim(str(get valuePairs 0))))
    (if (= nameToLook compareString)
      (do
        (def idToLook customerID))))
  (doseq [[salesID saleValuePairs] processedSaleData]
    (if (= (Integer/parseInt(str/trim(str(get saleValuePairs 0)))) idToLook)
      (do
        (def prodID (get saleValuePairs 1))
        (def prodCount (get saleValuePairs 2))
        (doseq [[productID valuePairProducts] processedProduct]
          (if (= productID (Integer/parseInt prodID))
            (do
              (def getValueProduct (get valuePairProducts 1))
              (def totalAmount (+ (* (Float/parseFloat getValueProduct) (Float/parseFloat prodCount)) totalAmount))))))))
  (println nameToLook ": $" totalAmount)
  99)
;It is here important to return an integer value as the JVM will throw a null pointer exception if there is no value returned.
;;==============================================================================================
;;CHOICE 5
;The computeTotal funciton is used to calculate the total number of products from the entered product input.
;The product ID is fetched from the processedProduct hashmap using the input product name which, in turn is given
;to the sales hashmap which using the productID calculates and add the corresponding product count to the result "totalNum"
(defn computeTotal [productToSearch]
  (def fetchID -1)
  (def totalNum 0)
  (def proSearch (str/trim productToSearch))
  (doseq [[productID valuePairProducts] processedProduct]
    (def truncatedItem (str/trim (str(get valuePairProducts 0))))
    (if ( = proSearch truncatedItem)
      (def fetchID (Integer/parseInt(str/trim(str productID))))))
  (doseq [[salesID saleValuePairs] processedSaleData]
    (def loopingID (Integer/parseInt(str/trim (str(get saleValuePairs 1)))))
    (def adder (Integer/parseInt(str/trim(str(get saleValuePairs 2)))))
    (if ( == loopingID fetchID)
      (def totalNum (+ totalNum adder))))
  (print productToSearch  ":" totalNum))

;;===============================================================================================


(defn mainFuncStarter[]
  (println "")
  (println "----------------------")
  (println "*** Sales Menu ***")
  (println "----------------------")
  (println "1.Display Customer Table")
  (println "2.Display Product Table")
  (println "3.Display Sales Table")
  (println "4.Total sales for Customer")
  (println "5.Total count for Sales")
  (println "6. Exit")
  (println "Enter your CHOICE: ")
  (println "")
  (fetchCustomerData)
  (fetchProductData)
  (fetchSalesData)
  (def choice (read))
  (if (or (< choice 1) (> choice 6))
    (println "Please enter the choice between 1 and 6. (INVALID OPTION)"))
  (if (== choice 1)
    (do (println "=====================================================")
        (println " ID |    NAME    |     ADDRESS     |    PHONE NO.  ")
        (println "=====================================================")
        (doseq [[customerID valuePairs] dividedDataPairs]
          (print customerID " :: ")
          (print (get valuePairs 0) " ")
          (print (get valuePairs 1) " ")
          (print (get valuePairs 2))
          (println))))
  (if (== choice 2)
    (do (println "==================")
        (println " ID | ITEM | COST")
        (println "==================")
        (doseq [[productID valuePairProducts] processedProduct]
          (print productID " :: ")
          (print (get valuePairProducts 0) " ")
          (print (get valuePairProducts 1))
          (println)
          (= (get valuePairProducts 1) (Float/parseFloat(str/trim(str(get valuePairProducts 1))))))))
  (if (== choice 3)
    (do (println "===============================")
        (println "ID |    NAME    | ITEM | VOLUME ")
        (println "===============================")
        (doseq [[salesID saleValuePairs] processedSaleData]
          (print salesID "::")
          (def  customerIDToBeReplaced (get saleValuePairs 0))
          (def  productIDToBeReplaced (get saleValuePairs 1))
          (def  numberOfProducts (get saleValuePairs 2))
          (findReplacements customerIDToBeReplaced productIDToBeReplaced)
          (print resultCustomer)
          (print " " resultProduct)
          (println "   " numberOfProducts)
          (= (get saleValuePairs 2) (Integer/parseInt(str/trim(str(get saleValuePairs 2))))))))
  (if (== choice 4)
    (do (print "Enter Customer name:") (flush) (read-line)
         (let[nameToLook (read-line)]
           (getTotalAmount nameToLook))))
  (if (== choice 5)
    (do (print "Enter product name:") (flush) (read-line)
        (let[productToSearch (read-line)]
          (computeTotal productToSearch))))
  (if (== choice 6)
    (do (println "Good Bye")
        (System/exit 0)))

  (mainFuncStarter))
(mainFuncStarter)

;;References:
;https://www.tutorialspoint.com/clojure/clojure_file_io.htm
;https://clojure.org/
;https://stackoverflow.com/questions/51809765/leiningen-throws-clojure-lang-persistentvector-cannot-be-cast-to-clojure-lang-na