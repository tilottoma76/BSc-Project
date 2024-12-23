package Classes;

import java.lang.*;
import Interface.CustomerOperation;


public class Customer implements CustomerOperation
{
	private int nid;
	private String name;
	private int age;
	private HotelRoom rooms[] = new HotelRoom[10];
		
	public Customer()
	{
		System.out.println("E-Customer");
	}
	public Customer(int nid, String name, int age)
	{
	
		this.nid = nid;
		this.name = name;
		this.age = age;
	}
	
	public void setNid(int nid){this.nid = nid;}
	public void setName(String name){this.name = name;}
	public void setAge(int age){this.age = age;}
	
	public int getNid(){return nid;}
	public String getName(){return name;}
	public int getAge(){return age;}
	
	public void showDetails()
	{
		System.out.println("Customer NID: " + nid);
		System.out.println("Customer Name: " + name);
		System.out.println("Customer Age: " + age);
		System.out.println();
	}
	
	public boolean insertRoom(HotelRoom a)
	{
		boolean flag = false;
		for(int i=0; i<rooms.length; i++)
		{
			if(rooms[i] == null)
			{
				rooms[i] = a;
				flag = true;
				break;
			}
		}
		return flag;
	}
	public boolean removeRoom(HotelRoom a)
	{
		boolean flag = false;
		for(int i=0; i<rooms.length; i++)
		{
			if(rooms[i] == a)
			{
				rooms[i] = null;
				flag = true;
				break;
			}
		}
		return flag;	
	}
	public void showAllRooms()
	{
		System.out.println("////////////////////");
		for(int i=0; i<rooms.length; i++)
		{
			if(rooms[i] != null)
			{
				rooms[i].showDetails();
				System.out.println();
			}
		}
	}
	public  HotelRoom searchRoom(int roomNumber)
	{
		HotelRoom a = null;
		for(int i = 0; i<rooms.length; i++)
		{
			if(rooms[i] != null)
			{
				if(rooms[i].getRoomNumber() == roomNumber)
				{
					a = rooms[i];
					break;
				}
			}
		}
		return a;
	}
	 

}