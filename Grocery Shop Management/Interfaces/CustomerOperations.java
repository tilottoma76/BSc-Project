package Interfaces;
import java.lang.*;
import Classes.*;

public interface CustomerOperations
{
	boolean insertCustomer(Customer c);
	boolean removeCustomer(Customer c);
	void showCustomerInfo();
}