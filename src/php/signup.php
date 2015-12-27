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
  $signup = json_decode($json);

  $lastname = $signup->{'lastname'};
  $firstname = $signup->{'firstname'};
  $email = $signup->{'email'};
  $password = $signup->{'password'};
  $country = $signup->{'country'};
  $city = $signup->{'city'};
  $zipcode = $signup->{'zipcode'};

  // requirements
  $sql = "SELECT * FROM User WHERE email='$email';";
  $result = $conn->query($sql);

  if($result->num_rows != 0) {
      echo '{ "ok" : false, "r_error" : { "e_email" : "this email is already in use", "e_default" : "" }}';
      exit();
  }

  $sql = "INSERT INTO User "
       . "SET lastname='$lastname',"
       .     "firstname='$firstname',"
       .     "email='$email',"
       .     "password='$password',"
       .     "country='$country',"
       .     "city='$city',"
       .     "zipcode='$zipcode';";
       

  if ($conn->query($sql) === TRUE) {
    echo '{ "ok" : true, "r_error" : { "e_email" : "", "e_default" : "" }}';
  } else {
    echo '{ "ok" : false, "r_error" : { "e_email" : "", "e_default" : "' .  $conn->error . '}}';
  }

  $conn->close();
?>