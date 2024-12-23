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
    public partial class FormRegistration : Form
    {
        private DataAccess Da {  get; set; }
        public FormRegistration()
        {
            InitializeComponent();
            this.Da = new DataAccess();
        }

        private void FormRegistration_FormClosed(object sender, FormClosedEventArgs e)
        {
            Application.Exit();
        }

        public void btnFormRegistrationSubmitButton_Click(object sender, EventArgs e)
        {
            try
            {
                if (String.IsNullOrEmpty(this.txtFormRegistrationName.Text) && String.IsNullOrEmpty(this.txtFormRegistrationPassword.Text))
                {
                    MessageBox.Show("Fill-up both UserName and Password");
                    return;
                }
                else
                {
                    string id = this.Da.AutoIdGenerate();

                    string sql1 = "INSERT INTO UserInfo Values ('" + id + "', '" + this.txtFormRegistrationName.Text + "', '" + this.txtFormRegistrationPassword.Text + "', '" + this.txtFormRegistrationRole.Text + "');";
                    var dt1 = this.Da.ExecuteDMLQuery(sql1);

                    MessageBox.Show("Your User ID is : "+  id +"","Allart", MessageBoxButtons.OK, MessageBoxIcon.Warning);

                    this.Hide();
                    FormLogin f1 = new FormLogin();
                    f1.Visible = true;
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
        }

        private void btnFormRegistrationCancelButton_Click(object sender, EventArgs e)
        {
            try
            {
                DialogResult Result = MessageBox.Show("Are you sure for cancel this registration Process ?", "Warning", MessageBoxButtons.YesNo, MessageBoxIcon.Question);

                if (Result == DialogResult.No)
                {
                    this.Show();
                    return;
                }

                this.Hide();
                FormLogin f1 = new FormLogin();
                f1.Show();
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
        }
    }
}
