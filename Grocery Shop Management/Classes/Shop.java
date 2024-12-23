package Classes;
import java.lang.*;
import Interfaces.*;

public class Shop implements CustomerOperations,ProductOperations
{
	public String shopAddress;
	public int number;
	public Customer customers[] = new Customer[400];
	public Product products[] = new Product[300];

	public void setShopAddress(String shopAddress)
	{
		this.shopAddress = shopAddress;
	}
	public void setNumber(int number)
	{
		this.number = number;
	}
	public String getShopAddress()
	{
		return shopAddress;
	}
	public int getNumber()
	{
		return number; 
	}
	public void showShop()
	{
		System.out.println("Shop Address: "+shopAddress);
		System.out.println("Shop Number: "+number);
	}

	public boolean insertCustomer(Customer c)
	{
		boolean flag = false;
		for(int i=0; i<customers.length; i++)
		{
			if(customers[i] == null)
			{
				customers[i] = c;
				flag = true;
				break;
			}
		}
		return flag;
	}

	public boolean removeCustomer(Customer c)
	{
		boolean flag = false;
		for(int i=0; i<customers.length; i++)
		{
			if(customers[i] == c)
			{
				customers[i] = null;
				flag =true;
				break;
			}
		}
		return flag;
		
	}
	public Customer searchCustomer(int phoneNumber)
	{
		Customer c = null;
		
		for(int i=0; i<customers.length; i++)
		{
			if(customers[i] != null)
			{
				if(customers[i].getPhoneNumber() == phoneNumber)
				{
					c = customers[i];
					break;
				}
			}
		}
		return c;
	}
	public void showCustomerInfo()
	{
		for(int i=0; i<customers.length; i++)
		{
			if(customers[i] != null)
			{
				System.out.println("_________________");
				customers[i].showCustomer();
				System.out.println("///////////////////");
			}
		}
	}
	public Customer getCustomer(int phoneNumber)
	{
		Customer c = null;
		for(int i=0; i<customers.length; i++)
		{
			if(customers[i] != null)
			{
				if(customers[i].getPhoneNumber() == phoneNumber)
				{
					c = customers[i];
					break;
				}
			}
		}
		return c;
	}


	public void insertProduct(Product p)
	{
		int flag = 0;
		for(int i=0; i<products.length; i++)
		{
			if(products[i] == null)
			{
				products[i] = p;
				flag = 1;
				break;
			}
		}
		if(flag == 1){System.out.println("Inserted");}
		else{System.out.println("Not Inserted");} 
	}
	public void removeProduct(Product p)
	{
		int flag = 0;
		for(int i=0; i<products.length; i++)
		{
			if(products[i] == p)
			{
				products[i] = null;
				flag =1;
				break;
			}
		}
		if(flag==1){System.out.println("Removed");}
		else{System.out.println("Not Removed");}
	}
	public void showProductInfo()
	{
		for(int i=0; i<products.length; i++)
		{
			if(products[i] != null)
			{
				System.out.println("_________________");
				products[i].showProductInfo();
				System.out.println("///////////////////");
			}
		}
	}
	public Product getProduct(String name)
	{
		Product p = null;
		for(int i=0; i<products.length; i++)
		{
			if(customers[i] != null)
			{
				if(products[i].getName() == name)
				{
					p = products[i];
					break;
				}
			}
		}
		return p;
	}

	
	












}