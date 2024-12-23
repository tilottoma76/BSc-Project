import java.lang.*;
import Classes.*;
import fileio.*;
import java.util.Scanner;

public class Start
{
	public static void main(String args[])
	{
		Scanner sc = new Scanner(System.in);

		String user, pass;
		System.out.println("----------------------------");
		System.out.println("Enter username Please");
		user = sc.nextLine();
		System.out.println("----------------------------");
		System.out.println("Enter password Please");
		pass = sc.nextLine();
		if(user.equals("Hotel")&& pass.equals("xyzabc"))
		{
			System.out.println("Successful");
		}
		else
		{
			System.out.println("Sorry! Can't access.");
		}

		Hotel b = new Hotel();
		FileReadWriteDemo frwd = new FileReadWriteDemo();
		
		System.out.println("-------------------------------------------");
		System.out.println("Welcome to Teams Hotel");
		System.out.println("-------------------------------------------");
		
		boolean repeat = true;
		
		while(repeat)
		{
			System.out.println("What do you want to do?");
			System.out.println("\t1. Employee Management");
			System.out.println("\t2. Customer Management");
			System.out.println("\t3. Customer-Room Management");
			System.out.println("\t4. Room Transactions");
			System.out.println("\t5. Exit");
			
			System.out.println();
			System.out.print("What is you choice: ");
			int choice = sc.nextInt();
			
			switch(choice)
			{
				case 1:
					
					System.out.println("-------------------------------------------");
					System.out.println("You have chose Employee Management");
					System.out.println("-------------------------------------------");
					
					System.out.println();
					System.out.println("You have the following options:");
					System.out.println("\t1. Insert New Employee");
					System.out.println("\t2. Remove Existing Employee");
					System.out.println("\t3. Search an Employee");
					System.out.println("\t4. Show All Employees");
					System.out.println("\t5. Go Back");
					
					System.out.println();
					System.out.print("Enter you option: ");
					int option1 = sc.nextInt();
					
					switch(option1)
					{
						case 1:
						
							System.out.println("-------------------------------------------");
							System.out.println("You have chose to Insert New Employee");
							System.out.println("-------------------------------------------");
							
							System.out.print("Enter Employee ID: ");
							String empId1 = sc.next();
							System.out.print("Enter Employee Name: ");
							String name1 = sc.next();
							System.out.print("Enter Employee Salary: ");
							double salary1 = sc.nextDouble();
							
							Employee e1 = new Employee();
							e1.setEmpId(empId1);
							e1.setName(name1);
							e1.setSalary(salary1);
							
							if(b.insertEmployee(e1))
							{
								System.out.println("Employee Id: " + e1.getEmpId() + " has been Inserted");
							}
							else
							{
								System.out.println("Employee Id: " + e1.getEmpId() + " can Not be Inserted");
							}
							System.out.println();
							break;
							
						case 2:
						
							System.out.println("-------------------------------------------");
							System.out.println("You have chose to Remove an Employee");
							System.out.println("-------------------------------------------");
							
							System.out.print("Enter an Employee ID for Removing: ");
							String empId2 = sc.next();
							
							Employee e2 = b.searchEmployee(empId2);
							
							if(e2 != null)
							{
								System.out.println("### Employee Found for Removing ###");
								if(b.removeEmployee(e2))
								{
									System.out.println("Employee Id: " + e2.getEmpId() + " has been Removed");
								}
							}
							else
							{
								System.out.println("### Employee NOT Found and Can NOT be Removed ###");
							}
							System.out.println();
							break;
							
						case 3: 
						
							System.out.println("-------------------------------------------");
							System.out.println("You have chose to Search an Employee");
							System.out.println("-------------------------------------------");
							
							System.out.print("Enter an Employee ID for searching: ");
							String empId3 = sc.next();
							
							Employee e3 = b.searchEmployee(empId3);
							
							if(e3 != null)
							{
								System.out.println("### Employee Found ###");
								System.out.println("Employee ID: " + e3.getEmpId());
								System.out.println("Employee Name: " + e3.getName());
								System.out.println("Employee Salary: " + e3.getSalary());
							}
							else
							{
								System.out.println("### Employee NOT Found ###");
							}
							System.out.println();
							break;
							
						case 4:
							
							System.out.println("-------------------------------------------");
							System.out.println("You have chose to Show All Employees");
							System.out.println("-------------------------------------------");
							
							b.showAllEmployees();
							
							break;
							
						case 5:
						
							System.out.println("-------------------------------------------");
							System.out.println("You have chose to Go Back");
							System.out.println("-------------------------------------------");
							break;
							
						default:
						
							System.out.println("-------------------------------------------");
							System.out.println("Invalid Option");
							System.out.println("-------------------------------------------");
							break;
					}
					System.out.println();
					break;
					
				case 2:
				
					System.out.println("-------------------------------------------");
					System.out.println("You have chose Customer Management");
					System.out.println("-------------------------------------------");
					
					System.out.println();
					System.out.println("You have the following options:");
					System.out.println("\t1. Insert New Customer");
					System.out.println("\t2. Remove Existing Customer");
					System.out.println("\t3. Search a Customer");
					System.out.println("\t4. Show All Customers");
					System.out.println("\t5. Go Back");
					
					System.out.println();
					System.out.print("Enter you option: ");
					int option2 = sc.nextInt();
					
					switch(option2)
					{
						case 1:
						
							System.out.println("-------------------------------------------");
							System.out.println("You have chose to Insert New Customer");
							System.out.println("-------------------------------------------");
							
							System.out.print("Enter Customer NID: ");
							int nid1 = sc.nextInt();
							System.out.print("Enter Customer Name: ");
							String name1 = sc.next();
							System.out.print("Enter Customer Age: ");
							int age1 = sc.nextInt();
							
							Customer c1 = new Customer(nid1, name1, age1);
							
							if(b.insertCustomer(c1))
							{
								System.out.println("Customer Nid: " + c1.getNid() + " has been Inserted");
							}
							else
							{
								System.out.println("Customer Nid: " + c1.getNid() + " Can NOT be Inserted");
							}
							System.out.println();
							break;
							
						case 2:
						
							System.out.println("-------------------------------------------");
							System.out.println("You have chose to Remove a Customer");
							System.out.println("-------------------------------------------");
							System.out.print("Enter Customer NID: ");
							int nid2 = sc.nextInt();
							
							Customer c2 = b.searchCustomer(nid2);
							
							if(c2 != null)
							{
								System.out.println("### Customer Found for Removing ###");
								if(b.removeCustomer(c2))
								{
									System.out.println("Customer Nid: " + c2.getNid() + " has been Removed");
								}
							}
							else
							{
								System.out.println("### Customer NOT Found and Can NOT be Removed ###");
							}
							System.out.println();
							
							break;
							
						case 3: 
						
							System.out.println("-------------------------------------------");
							System.out.println("You have chose to Search a Customer");
							System.out.println("-------------------------------------------");
							
							System.out.print("Enter Customer NID: ");
							int nid3 = sc.nextInt();
							
							Customer c3 = b.searchCustomer(nid3);
							
							if(c3 != null)
							{
								System.out.println("### Customer Found  ###");
								c3.showDetails();
							}
							else
							{
								System.out.println("### Customer NOT Found ###");
							}
							System.out.println();
							
							break;
							
						case 4:
							
							System.out.println("-------------------------------------------");
							System.out.println("You have chose to Show All Customers");
							System.out.println("-------------------------------------------");
							
							b.showAllCustomers();
							
							break;
							
						case 5:
						
							System.out.println("-------------------------------------------");
							System.out.println("You have chose to Go Back");
							System.out.println("-------------------------------------------");
							break;
							
						default:
						
							System.out.println("-------------------------------------------");
							System.out.println("Invalid Option");
							System.out.println("-------------------------------------------");
							break;
					}
					System.out.println();
					
					break;
				
				
				case 3:
					
					System.out.println("-------------------------------------------");
					System.out.println("You have chose Customer Room Management");
					System.out.println("-------------------------------------------");
					
					System.out.println();
					System.out.println("You have the following options:");
					System.out.println("\t1. Insert New Room  for a Customer");
					System.out.println("\t2. Show All Room of a Customer");
					System.out.println("\t3. Go Back");
					
					System.out.println();
					System.out.print("Enter you option: ");
					int option3 = sc.nextInt();
					
					switch(option3)
					{
						case 1:
						
							System.out.println("-------------------------------------------");
							System.out.println("You have chose to Insert a Room  for a Customer");
							System.out.println("-------------------------------------------");
							
							System.out.print("Enter Customer NID: ");
							int nid1 = sc.nextInt();
							
							Customer c1 = b.searchCustomer(nid1);
							
							if(c1 != null)
							{
								System.out.println("### Valid Customer ###");
								
								System.out.println("Which type of Room do you want?");
								System.out.println("\t1. Vip Room   ");
								System.out.println("\t2. AC Room    ");
								System.out.println("\t3. Non Ac Room ");
								System.out.println("\t4. Go Back  ");
								System.out.println();
								
								System.out.print("Your Type: ");
								int type = sc.nextInt();
								
								HotelRoom a = null;
								
								if(type == 1)
								{
									
									System.out.println("----------------------------------");
									System.out.println("Welcome to  Vip Room ");
									System.out.println("----------------------------------");
                                       
									System.out.print("Enter Room  Number : ");
									int an = sc.nextInt(); 
									System.out.print("Enter Room Holder Name : ");
									String ahn = sc.next();
									System.out.print("Enter Room Prize : ");
									double b1 = sc.nextDouble();
									 System.out.print("How many days stay : ");
					                   int st = sc.nextInt();
					                   

									VipRoom vp = new VipRoom();
									vp.setRoomNumber(an);
									vp.setRoomHolderName(ahn);
									vp.setprize(b1);
									vp.setroomType("Vip Room");
							        vp.setDays(st);
									a = vp;
								}
								else if(type == 2)
								{
									System.out.println("----------------------------------");
									System.out.println("Welcome to AC Room");
									System.out.println("----------------------------------");
									
									  
									System.out.print("Enter Room  Number : ");
								     int an = sc.nextInt();
									System.out.print("Enter Room Holder Name : ");
									String ahn = sc.next();
									System.out.print("Enter Room Prize : ");
									double b1 = sc.nextDouble();
								     System.out.print("How many days stay : ");
					                   int st = sc.nextInt();
									
									AcRoom ac = new AcRoom();
								   ac.setRoomNumber(an);
									ac.setRoomHolderName(ahn);
									ac.setprize(b1);
									ac.setroomTypeA("AC Room");
									ac.setDays(st);
									a = ac;
								}
								else if(type == 3)
								{
									System.out.println("----------------------------------");
									System.out.println("Welcome to Non AC Room");
									System.out.println("----------------------------------");
									
									  
									System.out.print("Enter Room  Number : ");
								     int an = sc.nextInt();
									System.out.print("Enter Room Holder Name : ");
									String ahn = sc.next();
									System.out.print("Enter Room Prize : ");
									double b1 = sc.nextDouble();
								     System.out.print("How many days stay : ");
					                   int st = sc.nextInt();
									
									NonAcRoom nc = new NonAcRoom();
								   nc.setRoomNumber(an);
									nc.setRoomHolderName(ahn);
									nc.setprize(b1);
									nc.setRoomTypeN("Non AC");
									nc.setDays(st);
									a = nc;
								}
								

								else if(type == 4)
								{
									System.out.println("----------------------------------");
									System.out.println("Going Back.....");
									System.out.println("----------------------------------");
								}
								else
								{
									System.out.println("----------------------------------");
									System.out.println("Invalid Type");
									System.out.println("----------------------------------");
								}
								
							
								if(a != null)
								{
									if(c1.insertRoom(a))
									{
										System.out.println("Room Number: "+ a.getRoomNumber()+" has been inserted for  Nid: " + c1.getNid());
									}
									else
									{
										System.out.println("Room Number: "+ a.getRoomNumber()+" can Not be inserted for Nid:  " + c1.getNid());
									}
								}
								
								
							}
							else
							{
								System.out.println("### Invalid Customer..Room  Can NOT be Reserve ###");
							}
							System.out.println();
							break;
							
						
							
						case 2:
							
							System.out.println("-------------------------------------------");
							System.out.println("You have chose to Show All Room for a Customer");
							System.out.println("-------------------------------------------");
							
							System.out.print("Enter Customer NID: ");
							int nid4 = sc.nextInt();
							
							Customer c4 = b.searchCustomer(nid4);
							
							if(c4 != null)
							{
								c4.showDetails();
								c4.showAllRooms();
							}
							else
							{
								System.out.println("### Invalid Customer ###");
							}
							System.out.println();
							break;
							
						case 3:
						
							System.out.println("-------------------------------------------");
							System.out.println("You have chose to Go Back");
							System.out.println("-------------------------------------------");
							break;
							
						default:
						
							System.out.println("-------------------------------------------");
							System.out.println("Invalid Option");
							System.out.println("-------------------------------------------");
							break;
					}
					System.out.println();
					break;

				case 4:
					
					System.out.println("-------------------------------------------");
					System.out.println("You have chose Transactions");
					System.out.println("-------------------------------------------");
					
					System.out.println();
					System.out.println("You have the following options:");
					System.out.println("\t1. Check Out and Pay Bill ");
					System.out.println("\t2. Show All Transactions");
					System.out.println("\t3. Go Back");
					
					System.out.println();
					System.out.print("Enter you option: ");
					int option4 = sc.nextInt();
					
					switch(option4)
					{
						case 1:
						
							System.out.println("-------------------------------------------");
							System.out.println("You have chose Check Out For Customer");
							System.out.println("-------------------------------------------");
							
							System.out.print("Enter Customer NID: ");
							int nid1 = sc.nextInt();
							
							Customer c1 = b.searchCustomer(nid1);
							
							if(c1 != null)
							{
								System.out.println("### Valid Customer ###");
								
								System.out.print("Enter Room Number : ");
								int an = sc.nextInt();
							
								HotelRoom a = c1.searchRoom(an);

							
								
								if(a != null)
								{
									System.out.println("### Room Found  ###");
									
									System.out.println();
                                     System.out.println("You have the following options:");
					                 System.out.println("\t1. Dicount  ");
					                 System.out.println("\t2. Non Discount ");
					                 System.out.println("\t3. Go Back");

                                     System.out.print("Enter you option: ");
					                 int option5 = sc.nextInt();

					                switch(option5){
						                   case 1:

                                          	System.out.println("-------------------------------------------");
							                System.out.println("You have chose Coustomer has a Discount cupon ");
							                System.out.println("-------------------------------------------");
                                            System.out.print("Enter Discount Rate : ");
								              int  d = sc.nextInt();    
                                      
                                                    a.discountRate(d);
                                                   
                                             
                                           
                                           	frwd.writeInFile( " Nid number " +c1.getNid() + "has been checked out. For room number " + a.getRoomNumber() + ". Total cost "+ a.getprize()+" and Discount" + d +". Stays "+ a.getDays()+" days" );
                                          
                                       
                                        
                                        if(c1.removeRoom(a))
                                  {
                                  	System.out.println(" Room Number " + a.getRoomNumber() + "has been remove for Nid: "+ c1.getNid());

                                  }
									break;

									case 2:
									 	System.out.println("-------------------------------------------");
							                System.out.println("You have chose Coustomer has a No Discount cupon ");
							                System.out.println("-------------------------------------------");
                                                 
                                                System.out.println("Totl prize : " +  a.getprize()*a.getDays());
                                           
                                      frwd.writeInFile( " Nid number " +c1.getNid() + "has been checked out. For room number " + a.getRoomNumber() + ". Total cost "+ a.getprize()+". Stays "+ a.getDays()+" days" );
                                        
                                        if(c1.removeRoom(a))
                                  {
                                  	System.out.println(" Room Number " + a.getRoomNumber() + " has been remove for Nid:  "+ c1.getNid());

                                  }
                                  break;

							default:
							System.out.println("-------------------------------------------");
							System.out.println("Invalid Option");
							System.out.println("-------------------------------------------");
							break;


								
							}


						}
							else
								{
									System.out.println("### Invalid Room ###");
								}
							}
							else
							{
								System.out.println("### Invalid Customer ###");
							}
							System.out.println();
							break;
							
						case 2:
					
						
							System.out.println("-------------------------------------------");
							System.out.println("You have chose to See Transaction History");
							System.out.println("-------------------------------------------");
							
							frwd.readFromFile();
							
							break;
							
						case 3:
						
							System.out.println("-------------------------------------------");
							System.out.println("You have chose to Go Back");
							System.out.println("-------------------------------------------");
							break;
							
						default:
						
							System.out.println("-------------------------------------------");
							System.out.println("Invalid Option");
							System.out.println("-------------------------------------------");
							break;
					}
					System.out.println();
					
					break;
					
				case 5:
					
					System.out.println("-------------------------------------------");
					System.out.println("You have chose to Exit");
					System.out.println("-------------------------------------------");
					
					repeat = false;
					
					break;
					
					
				default:
					
					System.out.println("-------------------------------------------");
					System.out.println("Invalid Choice...");
					System.out.println("-------------------------------------------");
					
					break;
			}
		}
	}
}