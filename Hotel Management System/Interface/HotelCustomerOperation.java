package Interface;

import java.lang.*;
import Classes.Customer;

public interface HotelCustomerOperation
{
	boolean insertCustomer(Customer c);
	boolean removeCustomer(Customer c);
	Customer searchCustomer(int nid);
	void showAllCustomers();
}