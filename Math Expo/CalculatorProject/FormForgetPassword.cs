using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Data.SqlClient;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using WFADBApp;

namespace CalculatorProject
{
    public partial class FormForgetPassword : Form
    {
        private DataAccess Da {  get; set; }
        private string FormLoginUserID { get; set; }
        public FormForgetPassword()
        {
            InitializeComponent();
            this.Da = new DataAccess();
        }

        public FormForgetPassword(string userID) : this()
        {
            try
            {
                FormLoginUserID = userID;
            }
            catch(Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
        }

        private void FormForgetPassword_FormClosed(object sender, FormClosedEventArgs e)
        {
            Application.Exit();
        }

        private void btnFormForgetPasswordDone_Click(object sender, EventArgs e)
        {
            try
            {
                if (FormLoginUserID == this.txtFormForgetPasswordUserID.Text)
                {
                    if (this.txtFormForgetPasswordNewPassword.Text == this.txtFormForgetPasswordConfirmPassword.Text)
                    {
                        string sql = "update UserInfo set Password = '" + this.txtFormForgetPasswordConfirmPassword.Text + "' where ID = '" + this.txtFormForgetPasswordUserID.Text + "';";

                        var ds = this.Da.ExecuteDMLQuery(sql);

                        MessageBox.Show("\tSuccessful \nPassword has been reseted");

                        this.Hide();
                        FormLogin f1 = new FormLogin();
                        f1.Visible = true;
                    }
                    else
                    {
                        MessageBox.Show("Password not matched \nGive password Properly");
                    }
                }
                else
                {
                    MessageBox.Show("User ID not matched");
                }

            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message.ToString());
            }
        }

        private void btnFormForgetPasswordCancel_Click(object sender, EventArgs e)
        {
            DialogResult Result = MessageBox.Show("Don't you want change password ?", "Warning", MessageBoxButtons.YesNo, MessageBoxIcon.Question);
            
            if (DialogResult == DialogResult.No)
                return;

            this.Hide();
            FormLogin f1 = new FormLogin();
            f1.Show();
        }
    }
}
