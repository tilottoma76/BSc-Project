import java.lang.*;
import Classes.*;
import Interfaces.*;
import java.util.Scanner;
import java.time.LocalDate;

public class Start
{
	public static void main(String []arg)
	{
		LocalDate myobj = LocalDate.now();
		System.out.println("Todays date: "+myobj);
		Scanner ri = new Scanner(System.in);
		//Product p1= new Product();
		Shop sh1 = new Shop();
		System.out.println("__________________________________");
		System.out.println("Welcome to Raisa Grocery Shop");
		System.out.println("__________________________________");

		boolean repeat = true;
		while(repeat)
		{
			System.out.println("You have the follwing option:");
			System.out.println("\t1.Shop Management");
			System.out.println("\t2.Product Management");
			System.out.println("\t3.Customer Management");
			System.out.println("\t4.Billing Product");
			System.out.println("\t5.Exit");
			System.out.println("");
			System.out.println("What is your choice:  ");
			int choice = ri.nextInt();

			switch(choice)
			{
				case 1:

				System.out.println("-----------------------------");
				System.out.println("You have chosen Shop Management");
				System.out.println("---------------------------------");

				System.out.println("Chose your option");
				System.out.println("\t1.ShopInformation");
				System.out.println("\t2.Go back");

				System.out.println("");
				int option1 = ri.nextInt();

				switch(option1)
				{
					case 1:
					System.out.println("-----------------------------");
					System.out.println("You have chosen Shop Information");
					System.out.println("-----------------------------");
					sh1.setShopAddress( "Bosundhora road 2");
					sh1.setNumber(38746787);
					sh1.showShop();
					break;

					case 2:
					System.out.println("-----------------------------");
					System.out.println("You have chosen go back");
					System.out.println("-----------------------------");
					break;

					default:

					System.out.println("-----------------------------");
					System.out.println("Invalid choice");
					System.out.println("-----------------------------");
					break;
					
				}
				System.out.println("");
				break;


			case 2:
			System.out.println("-----------------------------");
			System.out.println("You have chosen Product Management");
			System.out.println("-----------------------------");

			System.out.println("You have the following options");
			System.out.println("\t1.Rice Product");
			System.out.println("\t2.Bread Product");
			System.out.println("\t3.Suger Product");
			System.out.println("\t4.Salt Product");
			System.out.println("\t5.Onion Product");
			System.out.println("\t6.Oil Product");
			System.out.println("\t7.Tomato Product");
			System.out.println("\t8.Sauce Product");
			System.out.println("\t9.Potato Product");
			System.out.println("\t10.Pasta Product");
			System.out.println("\t11.Tea Product");
			System.out.println("\t12.Egg Product");
			System.out.println("\t13.Go back");


			System.out.println("");
			System.out.println("Enter Your Option:");
			int option2=ri.nextInt();

			switch(option2)
			{
				case 1:
				System.out.println("--------------------------------------");
				System.out.println(" You have chosen rice product");
				System.out.println("--------------------------------------");
				
				Rice r1 = new Rice();
				r1.setName("ChiniguraRice");
				r1.getName();
				r1.setPrice(100);
				r1.setNetWeight(1);
				r1.setVat(20);
				r1.weightInfo();
				r1.showProductInfo();
				break;

				case 2:
				System.out.println("--------------------------------------");
				System.out.println(" You have chosen bread product");
				System.out.println("--------------------------------------");

				Bread b1 = new Bread();
				b1.setName("BananaBread");
				b1.getName();
				b1.setPrice(80);
				b1.setCountNumber(1);
				b1.countNumberInfo();
				b1.setVat(10);
				b1.discoundPercentage(10);
				b1.showProductInfo();
				break;

				case 3:
				System.out.println("--------------------------------------");
				System.out.println(" You have chosen suger product");
				System.out.println("--------------------------------------");

				Suger s1 = new Suger();
				s1.setName("BrownSuger");
				s1.getName();
				s1.setPrice(250);
				s1.setNetWeight(1);
				s1.weightInfo();
				s1.setVat(12);
				s1.discoundPercentage(11);
				s1.showProductInfo();
				break;

				case 4:
				System.out.println("--------------------------------------");
				System.out.println(" You have chosen salt product");
				System.out.println("--------------------------------------");

				Salt sl1 = new Salt();
				sl1.setName("WhiteSalt");
				sl1.getName();
				sl1.setPrice(350);
				sl1.setNetWeight(1);
				sl1.weightInfo();
				sl1.setVat(13);
				sl1.discoundPercentage(9);
				sl1.showProductInfo();
				break;

				case 5:
				System.out.println("--------------------------------------");
				System.out.println(" You have chosen onion product");
				System.out.println("--------------------------------------");

				Onion o1 = new Onion();
				o1.setName("SpanishOnion");
				o1.getName();
				o1.setPrice(150);
				o1.setNetWeight(1);
				o1.weightInfo();
				o1.setVat(15);
				o1.discoundPercentage(12);
				o1.showProductInfo();
				break;

				case 6:
				System.out.println("--------------------------------------");
				System.out.println(" You have chosen oil product");
				System.out.println("--------------------------------------");

				Oil oi1 =new Oil();
				oi1.setName("OliveOil");
				oi1.getName();
				oi1.setPrice(550);
				oi1.setNetWeight(1);
				oi1.weightInfo();
				oi1.setVat(8);
				oi1.discoundPercentage(14);
				oi1.showProductInfo();
				break;

				case 7:
				System.out.println("--------------------------------------");
				System.out.println(" You have chosen tomato product");
				System.out.println("--------------------------------------");


				Tomato t1 = new Tomato();
				t1.setName("GreenTomato");
				t1.getName();
				t1.setPrice(50);
				t1.setNetWeight(1);
				t1.weightInfo();
				t1.setVat(2.5);
				t1.discoundPercentage(6);
				t1.showProductInfo();
				break;

				case 8:
				System.out.println("--------------------------------------");
				System.out.println(" You have chosen sauce product");
				System.out.println("--------------------------------------");


				Sauce sa1 = new Sauce();
				sa1.setName("TomatoSauce");
				sa1.getName();
				sa1.setPrice(380);
				sa1.setCountNumber(1);
				sa1.countNumberInfo();
				sa1.setVat(5.67);
				sa1.discoundPercentage(8);
				sa1.showProductInfo();
				break;

				case 9:
				System.out.println("--------------------------------------");
				System.out.println(" You have chosen potato product");
				System.out.println("--------------------------------------");


				Potato po1 = new Potato();
				po1.setName("SweetPotato");
				po1.getName();
				po1.setPrice(200);
				po1.setNetWeight(1);
				po1.weightInfo();
				po1.setVat(8.5);
				po1.discoundPercentage(7);
				po1.showProductInfo();
				break;

				case 10:
				System.out.println("--------------------------------------");
				System.out.println(" You have chosen pasta product");
				System.out.println("--------------------------------------");

				Pasta pa1 = new Pasta();
				pa1.setName("ItalianPasta");
				pa1.getName();
				pa1.setPrice(300);
				pa1.setNetWeight(1);
				pa1.weightInfo();
				pa1.setVat(8.67);
				pa1.discoundPercentage(8.2);
				pa1.showProductInfo();
				break;

				case 11:

				System.out.println("--------------------------------------");
				System.out.println(" You have chosen tea product");
				System.out.println("--------------------------------------");


				Tea te1 = new Tea();

				te1.setName("GreenTea");
				te1.getName();
				te1.setPrice(700);
				te1.setNetWeight(1);
				te1.weightInfo();
				te1.setVat(8.57);
				te1.discoundPercentage(6.2);
				te1.showProductInfo();
				break;

				case 12:

				System.out.println("--------------------------------------");
				System.out.println(" You have chosen egg product");
				System.out.println("--------------------------------------");

				Egg e1 = new Egg();
				e1.setName("Egg");
				e1.getName();
				e1.setPrice(120);
				e1.setCountNumber(12);
				e1.countNumberInfo();
				e1.setVat(0.0);
				e1.discoundPercentage(0.0);
				e1.showProductInfo();

				case 13:
					System.out.println("-----------------------------");
					System.out.println("You have chosen go back");
					System.out.println("-----------------------------");
					break;

					default:

					System.out.println("-----------------------------");
					System.out.println("Invalid choice");
					System.out.println("-----------------------------");
					break;
					

			}
			System.out.println("");
			break;


			case 3:
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
					int option3 = ri.nextInt();
					
					switch(option3)
					{
						case 1:
						
							System.out.println("-------------------------------------------");
							System.out.println("You have chose to Insert New Customer");
							System.out.println("-------------------------------------------");
					
							System.out.print("Enter Customer Name: ");
							String name1 = ri.next();
							System.out.print("Enter Customer Phone Number: ");
							int phn1 = ri.nextInt();
							
							Customer c1 = new Customer(name1, phn1);
						
							
							if(sh1.insertCustomer(c1))
							{
								System.out.println("Customer phone number: " + c1.getPhoneNumber() + " has been Inserted");
							}
							else
							{
								System.out.println("Customer phone number: " + c1.getPhoneNumber() + " Can NOT be Inserted");
							}
							System.out.println();
							break;
							
						case 2:
						
							System.out.println("-------------------------------------------");
							System.out.println("You have chose to Remove a Customer");
							System.out.println("-------------------------------------------");
							System.out.println("Enter Customer phone number: ");
							int phn2 = ri.nextInt();
							
							Customer c2 = sh1.searchCustomer(phn2);
							
							if(c2 != null)
							{
								System.out.println("### Customer Found for Removing ###");
								if(sh1.removeCustomer(c2))
								{
									System.out.println("Customer Nid: " + c2.getPhoneNumber() + " has been Removed");
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
							
							System.out.println("Enter Customer phone number: ");
							int phn3 = ri.nextInt();
							
							Customer c3 = sh1.searchCustomer(phn3);
							
							if(c3 != null)
							{
								System.out.println("### Customer Found  ###");
								c3.showCustomer();
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
							
							sh1.showCustomerInfo();
							
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



				case 4:

				System.out.println("-------------------------------------------");
				System.out.println("You have chosen billing option");
				System.out.println("-------------------------------------------");

				System.out.println();
				System.out.println("You have the following option");
				System.out.println("\t1.Rice billing");
				System.out.println("\t2.Bread billing");
				System.out.println("\t3.Suger billing");
				System.out.println("\t4.Salt billing");
				System.out.println("\t5.Onion billing");
				System.out.println("\t6.Oil billing");
				System.out.println("\t7.Tomato billing");
				System.out.println("\t8.Sauce billing");
				System.out.println("\t9.Potato billing");
				System.out.println("\t10.Pasta billing");
				System.out.println("\t11.Tea billing");
				System.out.println("\t12.Egg billing");
				System.out.println("\t13.Go back");

				System.out.println();
				System.out.println("Enter your option: ");
				int option4 = ri.nextInt();

				Product pro1 = null;

				switch(option4)
				{
					case 1:

					System.out.println("-------------------------------------------");
					System.out.println("You have chosen rice billing");
					System.out.println("-------------------------------------------");

					System.out.println("Rice price is 100 inter 100");
					double x1 = ri.nextDouble();
					System.out.println("Enter the kg: ");
					double z1 = ri.nextDouble();

					Product ro1 = new Product();
					ro1.setPrice(x1);
					ro1.setNetWeight(z1);
					ro1.billingProduct();

					pro1=ro1;

					break;

					case 2:

					System.out.println("-------------------------------------------");
					System.out.println("You have chosen bread billing");
					System.out.println("-------------------------------------------");

					System.out.println("Bread price is 80 inter 80");
					double x2 = ri.nextDouble();
					System.out.println("Enter the number you want to buy bread: ");
					double z2 = ri.nextDouble();

					Product ro2 = new Product();
					ro2.setPrice(x2);
					ro2.setNetWeight(z2);
					ro2.billingProduct();

					pro1=ro2;

					break;

					case 3:
					System.out.println("-------------------------------------------");
					System.out.println("You have chosen brown suger billing");
					System.out.println("-------------------------------------------");

					System.out.println("Brown Suger price is 250 inter 250");
					double x3 = ri.nextDouble();
					System.out.println("Enter the kg: ");
					double z3 = ri.nextDouble();

					Product ro3 = new Product();
					ro3.setPrice(x3);
					ro3.setNetWeight(z3);
					ro3.billingProduct();

					pro1=ro3;

					break;

					case 4:

					System.out.println("-------------------------------------------");
					System.out.println("You have chosen salt billing");
					System.out.println("-------------------------------------------");

					System.out.println("Salt price is 350 inter 350");
					double x4 = ri.nextDouble();
					System.out.println("Enter the kg: ");
					double z4 = ri.nextDouble();

					Product ro4 = new Product();
					ro4.setPrice(x4);
					ro4.setNetWeight(z4);
					ro4.billingProduct();

					pro1=ro4;

					break;

					case 5:

					System.out.println("-------------------------------------------");
					System.out.println("You have chosen Spanish Onion billing");
					System.out.println("-------------------------------------------");

					System.out.println("SpanishOnion price is 150 inter 150");
					double x5 = ri.nextDouble();
					System.out.println("Enter the kg: ");
					double z5 = ri.nextDouble();

					Product ro5 = new Product();
					ro5.setPrice(x5);
					ro5.setNetWeight(z5);
					ro5.billingProduct();

					pro1=ro5;

					break;

					case 6:

					System.out.println("-------------------------------------------");
					System.out.println("You have chosen Olive Oil billing");
					System.out.println("-------------------------------------------");

					System.out.println("Olive Oil price is 550 inter 550");
					double x6 = ri.nextDouble();
					System.out.println("Enter the kg: ");
					double z6 = ri.nextDouble();

					Product ro6 = new Product();
					ro6.setPrice(x6);
					ro6.setNetWeight(z6);
					ro6.billingProduct();

					pro1=ro6;

					break;

					case 7:

					System.out.println("-------------------------------------------");
					System.out.println("You have chosen Green Tomato billing");
					System.out.println("-------------------------------------------");

					System.out.println("Green Tomato price is 50 inter 50");
					double x7 = ri.nextDouble();
					System.out.println("Enter the kg: ");
					double z7 = ri.nextDouble();

					Product ro7 = new Product();
					ro7.setPrice(x7);
					ro7.setNetWeight(z7);
					ro7.billingProduct();

					pro1=ro7;

					break;

					case 8:

					System.out.println("-------------------------------------------");
					System.out.println("You have chosen Tomato Sauce billing");
					System.out.println("-------------------------------------------");

					System.out.println("Tomato Sauce price is 380 inter 380");
					double x8 = ri.nextDouble();
					System.out.println("Enter the kg: ");
					double z8 = ri.nextDouble();

					Product ro8 = new Product();
					ro8.setPrice(x8);
					ro8.setNetWeight(z8);
					ro8.billingProduct();

					pro1=ro8;

					break;

					case 9:

					System.out.println("-------------------------------------------");
					System.out.println("You have chosen Sweet Potato billing");
					System.out.println("-------------------------------------------");

					System.out.println("Sweet Potato price is 200 inter 200");
					double x9 = ri.nextDouble();
					System.out.println("Enter the kg: ");
					double z9 = ri.nextDouble();

					Product ro9 = new Product();
					ro9.setPrice(x9);
					ro9.setNetWeight(z9);
					ro9.billingProduct();

					pro1=ro9;

					break;

					case 10:

					System.out.println("-------------------------------------------");
					System.out.println("You have chosen Italian Pasta billing");
					System.out.println("-------------------------------------------");

					System.out.println("Italian Pasta price is 300 inter 300");
					double x10 = ri.nextDouble();
					System.out.println("Enter the kg: ");
					double z10 = ri.nextDouble();

					Product ro10 = new Product();
					ro10.setPrice(x10);
					ro10.setNetWeight(z10);
					ro10.billingProduct();

					pro1=ro10;

					break;

					case 11:

					System.out.println("-------------------------------------------");
					System.out.println("You have chosen Green Tea billing");
					System.out.println("-------------------------------------------");

					System.out.println("Green Tea price is 700 inter 700");
					double x11 = ri.nextDouble();
					System.out.println("Enter the kg: ");
					double z11 = ri.nextDouble();

					Product ro11 = new Product();
					ro11.setPrice(x11);
					ro11.setNetWeight(z11);
					ro11.billingProduct();

					pro1=ro11;

					break;

					case 12:

					System.out.println("-------------------------------------------");
					System.out.println("You have chosen egg billing");
					System.out.println("-------------------------------------------");

					System.out.println("Egg price is 120 inter 120");
					double x12 = ri.nextDouble();
					System.out.println("Enter the dozen: ");
					double z12 = ri.nextDouble();

					Product ro12 = new Product();
					ro12.setPrice(x12);
					ro12.setNetWeight(z12);
					ro12.billingProduct();

					pro1=ro12;

					break;

					case 13:

					System.out.println("-----------------------------");
					System.out.println("You have chosen go back");
					System.out.println("-----------------------------");
					break;

					default:

					System.out.println("-----------------------------");
					System.out.println("Invalid choice");
					System.out.println("-----------------------------");
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