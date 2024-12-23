using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Threading;
using System.Data.SqlClient;
using WFADBApp;

namespace CalculatorProject
{
    public partial class Form1 : Form
    {
        private DataAccess Da {  get; set; }
        private string ID {  get; set; }

        public Form1()
        {
            InitializeComponent();
            this.Da = new DataAccess();            
        }

        public Form1(string user_ID) : this()
        {
            this.ID = user_ID;
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            try
            {
                string sql = "select * from UserInfo where ID = '" + this.ID + "'and Role = 'admin';";

                DataSet ds = this.Da.ExecuteQuery(sql);

                if (ds.Tables[0].Rows.Count == 1)
                {
                    this.pnlButtonDataBase.Visible = true;
                }
                else
                {
                    this.pnlButtonDataBase.Visible = false;
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
            
        }

        private void Form1_FormClosed(object sender, FormClosedEventArgs e)
        {
            Application.Exit();
        }

        private void btnSlide_Click(object sender, EventArgs e)
        {
            try
            {
                if (this.pnlMenuVartical.Width == 56)
                {
                    this.pnlMenuVartical.Width = 200;
                }
                else
                    this.pnlMenuVartical.Width = 56;
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
        }

        private void btnConversion_Click(object sender, EventArgs e)
        {
            try
            {
                pnlMenuVartical.Width = 200;
                if (this.pnlConvertionContainer.Height == 0)
                {
                    this.pnlConvertionContainer.Height = 510;
                }
                else
                {
                    this.pnlConvertionContainer.Height = 0;
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
        }

        private void btnForm1LogOut_Click(object sender, EventArgs e)
        {
            try
            {
                pnlMenuVartical.Width = 200;

                this.Hide();
                FormLogin f1 = new FormLogin();
                f1.Show();
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, "Error");
            }
        }

        private void CallAccrossButton (object sender)
        {
            try
            {
                if (this.pnlForm1CenterDisplay.Controls.Count > 0)
                {
                    this.pnlForm1CenterDisplay.Controls.RemoveAt(0);
                }
                Form f1 = sender as Form;
                f1.TopLevel = false;
                f1.Dock = DockStyle.Fill;
                this.pnlForm1CenterDisplay.Controls.Add(f1);
                this.pnlForm1CenterDisplay.Tag = f1;
                f1.Show();
            }
            catch(Exception ex)
            {
                MessageBox.Show(ex.Message, "Error");
            }
        }
        private void btnForm1Home_Click(object sender, EventArgs e)
        {
            try
            {
                this.CallAccrossButton(new FormHomePage());
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, "Error");
            }
        }
        private void btnForm1StandardCalculator_Click(object sender, EventArgs e)
        {
            try
            {
                this.CallAccrossButton(new FormCalculator());
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, "Error");
            }
        }

        private void btnForm1SbDiscount_Click(object sender, EventArgs e)
        {
            try
            {
                this.CallAccrossButton(new FormConvertionDiscount());
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message.ToString(), "Error");
            }
        }

        private void btnForm1SbVolume_Click(object sender, EventArgs e)
        {
            try
            {
                this.CallAccrossButton(new FormVolume());
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, "Error");
            }
        }

        private void btnForm1DateBase_Click(object sender, EventArgs e)
        {
            try
            {
                Form1DataBase f1 = new Form1DataBase();
                this.CallAccrossButton(f1);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, "Error");
            }
        }

        private void btnForm1SbArea_Click(object sender, EventArgs e)
        {
            try
            {
                FormConvertArea f1 = new FormConvertArea();
                this.CallAccrossButton(f1);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, "Error");
            }
        }

        private void btnForm1SbSpeed_Click(object sender, EventArgs e)
        {
            try
            {
                this.CallAccrossButton(new FormConvertionSpeed());
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message.ToString(), "Error");
            }
        }

        private void btnForm1SbBMI_Click(object sender, EventArgs e)
        {
            try
            {
                this.CallAccrossButton(new FormConvertionBMI());
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message.ToString(), "Error");
            }
        }

        private void btnForm1SbEnergy_Click(object sender, EventArgs e)
        {
            try
            {
                this.CallAccrossButton(new FormConvertionEnergy());
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message.ToString(), "Error");
            }
        }

        private void btnForm1SbTemp_Click(object sender, EventArgs e)
        {
            try
            {
                this.CallAccrossButton(new FormConvertionTemp());
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message.ToString(), "Error");
            }
        }

        private void btnForm1SbLength_Click(object sender, EventArgs e)
        {
            try
            {
                this.CallAccrossButton(new FormConvertionLength());
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message.ToString(), "Error");
            }
        }

        private void btnForm1SbTime_Click(object sender, EventArgs e)
        {
            try
            {
                this.CallAccrossButton(new FormConvertionTime());
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message.ToString(), "Error");
            }
        }

        private void btnForm1SbMass_Click(object sender, EventArgs e)
        {
            try
            {
                this.CallAccrossButton(new FormConvertionMass());
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message.ToString(), "Error");
            }
        }

        private void btnForm1SbCurrency_Click(object sender, EventArgs e)
        {
            try
            {
                this.CallAccrossButton(new FormConvertionCurrency());
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message.ToString(), "Error");
            }
        }
    }
}
