

 * The design doesn't optimize for duplicate keys, yet handles duplicates.
 * 
  design principles:
  
    * don't optimize for same keys, yet must be able to handle same keys.
    * assume values will be small (~128 bits); don't handle overflow 

  assume primitive of spark map-reduce framework / divide and conquer
