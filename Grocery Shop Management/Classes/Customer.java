package Classes;
import java.lang.*;
import Interfaces.*;

public class Customer implements ShopOperations
{
	public String customerName;
	public int phoneNumber;
	public Shop shops[] = new Shop[400];

	public Customer()
	{
		System.out.println("Empty");	
	}

	public Customer(String customerName, int phoneNumber)
	{
		this.customerName=customerName;
		this.phoneNumber=phoneNumber;
	}
public void setCustomerName(String customerName)
{
	this.customerName=customerName;
}
public void setPhoneNumber(int phoneNumber)
{
	this.phoneNumber=phoneNumber;
}
public String getCustomerName()
{
	return customerName;
}
public int getPhoneNumber()
{
	return phoneNumber;
}
public void showCustomer()
{
	System.out.println("Customer Name: "+customerName);
	System.out.println("Customer Phone Number: "+phoneNumber);
}

/*public void insertProduct(Products p)
	{
		int flag = 0;
		for(int i=0; i<product.length; i++)
		{
			if(products[i] == null)
			{
				products[i] = p;
				flag = 1;
				break;
			}
		}
		if(flag == 1){System.out.println("Inserted")}
		else{System.out.println("Not Inserted")} 
	}
	public void romoveProduct(Product p)
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
	}*/
	public void showShopInfo()
	{
		for(int i=0; i<shops.length; i++)
		{
			if(shops[i] != null)
			{
				System.out.println("_________________");
				shops[i].showShop();
				System.out.println("///////////////////");
			}
		}
	}


}
