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
    public partial class FormLogin : Form
    {
        private DataAccess Da {  get; set; }
        public FormLogin()
        {
            InitializeComponent();
            this.Da = new DataAccess();
        }

        private void txtFormLoginUserID_TextChanged(object sender, EventArgs e)
        {
            try
            {

                txtFormLoginUserID.ForeColor = Color.White;
            }
            catch { }
        }

        private void txtFormLoginPassword_TextChanged(object sender, EventArgs e)
        {
            try
            {

                txtFormLoginPassword.ForeColor = Color.White;
            }
            catch { }
        }

        private void txtFormLoginUserID_DoubleClick(object sender, EventArgs e)
        {
            txtFormLoginUserID.SelectAll();
        }

        private void txtFormLoginPassword_DoubleClick(object sender, EventArgs e)
        {

            txtFormLoginPassword.SelectAll();
        }

        private void btnFormLoginForgetPassword_Click(object sender, EventArgs e)
        {
            try
            {
                string sql = "select ID From UserInfo where ID = '" + this.txtFormLoginUserID.Text + "';";
                DataSet ds = this.Da.ExecuteQuery(sql);

                if (ds.Tables[0].Rows.Count == 1)
                {
                    this.Hide();
                    FormForgetPassword frm1 = new FormForgetPassword(this.txtFormLoginUserID.Text);
                    frm1.Visible = true;
                }
                else
                {
                    MessageBox.Show("\t Invalid ID \n Please give the id Properly");
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message.ToString());
            }
        }

        private void btnFormLoginLoginButton_Click(object sender, EventArgs e)
        {
            try
            {
                string sql = "select * from UserInfo where ID = '" + this.txtFormLoginUserID.Text + "' and Password = '" + this.txtFormLoginPassword.Text + "';";
                DataSet ds = this.Da.ExecuteQuery(sql);

                if (ds.Tables[0].Rows.Count == 1)
                {

                    this.Hide();
                    Form1 f1 = new Form1(this.txtFormLoginUserID.Text);
                    f1.Visible = true;
                }
                else
                {
                    MessageBox.Show("Invalid Login");
                }

            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message.ToString());
            }
        }

        private void btnFormLoginCreateNewAccount_Click(object sender, EventArgs e)
        {
            this.Hide();
            FormRegistration fr = new FormRegistration();
            fr.Visible = true;
        }

        private void FormLogin_FormClosed(object sender, FormClosedEventArgs e)
        {
            Application.Exit();
        }
    }
}
