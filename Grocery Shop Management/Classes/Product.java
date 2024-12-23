package Classes;
import java.lang.*;
import Interfaces.*;

public class Product
{
	private double price;
	private double netWeight;
	private double countNum;
	private double productDiscount;
	private double vat;
	private double bill;
	private String name;

	public void setPrice(double price)
	{
		this.price = price;
	}
	public void setNetWeight(double netWeight)
	{
		this.netWeight = netWeight;
	}
	public void setCountNumber(double countNum)
	{
		this.countNum = countNum;
	}
	public void setVat(double vat)
	{
		this.vat = vat;
	}
	public void setName(String name)
	{
		this.name= name;
	}


	public double getPrice()
	{
		return price;
	}
	public double getNetWeight()
	{
 		return netWeight;
	}
	public double getCountNumber()
	{
		return countNum;
	}
	public double getVat()
	{
		return vat;
	}
	public String getName()
	{
		return name;
	}

	public void discoundPercentage(double discountAmount)
	{
		System.out.println("Discount Amount: "+discountAmount);
		this.productDiscount= ((discountAmount*100)/this.price);
		System.out.println("Product Discount: "+productDiscount+"%");

	}
	public void weightInfo()
	{
		System.out.println("NetWeight: " +netWeight+ "kg");
	}
	public void countNumberInfo()
	{
		System.out.println("Count Number: "+countNum);
	}
	public void billingProduct()
	{
		this.bill = this.price * this.netWeight;
		System.out.println("Billing product: "+bill);
	}

	public void showProductInfo()
	{
		System.out.println("Name: "+name);
		System.out.println("Price: " +price+ "taka");
		System.out.println("Vat: " +vat+ "taka");
		System.out.println("____________________________________________");
	}


}