<?php
   if(empty($_GET["par"])) {
     exit();
   }

   $servername = "192.168.2.29";
   $username = "rafael";
   $password = "739148625";
   $db = "anythinglocal";

   // Create connection
$conn = new mysqli($servername, $username, $password, $db);

   // Check connection
   if ($conn->connect_error) {
     die("Connection failed: " . $conn->connect_error);
   }

   // get Info
  $json = $_GET["par"];
  $signin = json_decode($json);

  $username = $signin->{'username'};
  $password = $signin->{'password'};

 // requirements
  $sql = "SELECT * FROM User WHERE email='$username' AND password='$password';";
  $result = $conn->query($sql);

  if($result->num_rows != 0) {
      echo '{ "ok" : true, "r_error" : ""}';
  } else {
      echo '{ "ok" : false, "r_error" : "bad username or password"}';
  }



  $conn->close();
?>