<?php
   $servername = "192.168.2.29";
   $username = "rafael";
   $password = "739148625";

   // Create connection
   $conn = new mysqli($servername, $username, $password);

   // Check connection
   if ($conn->connect_error) {
     die("Connection failed: " . $conn->connect_error);
   }

  echo "Connected successfully";

?>