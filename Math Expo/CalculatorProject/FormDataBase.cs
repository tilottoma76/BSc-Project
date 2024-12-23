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
    public partial class Form1DataBase : Form
    {
        private DataAccess Da {  get; set; }

        public Form1DataBase()
        {
            InitializeComponent();
            this.Da = new DataAccess();
            this.autoDataBaseCall();
        }

        private void Form1DataBase_Load(object sender, EventArgs e)
        {
            this.dgvUserInfo.ClearSelection();
        }

        private void ClearAllTextInsidePnlFormDBAddUpdate() // clear all textbox
        {
            try
            {
                this.txtFormDataBaseID.Clear();
                this.txtFormDataBaseUserName.Clear();
                this.txtFormDataBasePassword.Clear();
                this.cmbFormDataBaseRole.SelectedIndex = -1;

                this.txtFormDbSearch.Clear();
            }
            catch(Exception ex)
            {
                MessageBox.Show(ex.Message.ToString());
            }
        }

        private void autoDataBaseCall() // auto database call
        {
            try
            {
                string sql = "select * from UserInfo;";
                DataSet ds = this.Da.ExecuteQuery(sql);

                this.dgvUserInfo.AutoGenerateColumns = false;
                this.dgvUserInfo.DataSource = ds.Tables[0];
            }
            catch(Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
        }
        private void btnFormDbSearch_Click(object sender, EventArgs e) // search box search
        {
            try
            {
                string sql = "select * from UserInfo where Name = '" + this.txtFormDbSearch.Text.ToLower() + "';";

                var ds = this.Da.ExecuteQuery(sql);

                this.dgvUserInfo.AutoGenerateColumns = false;
                this.dgvUserInfo.DataSource = ds.Tables[0];
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
        }

        private bool IsValidToSave() // check text box are empty
        {
            if (string.IsNullOrEmpty(this.txtFormDataBaseID.Text) || string.IsNullOrEmpty(this.txtFormDataBaseUserName.Text) || string.IsNullOrEmpty(this.txtFormDataBasePassword.Text) || string.IsNullOrEmpty(this.cmbFormDataBaseRole.Text))
                return false;
            return true;
        }

        private void btnFormDataBaseSaveButton_Click(object sender, EventArgs e)
        {
            try
            {
                if (!IsValidToSave())
                {
                    MessageBox.Show("Fill-up all the items");
                    return;
                }
                //update
                var sql = "select * from UserInfo where ID = '"+ this.txtFormDataBaseID.Text +"';";
                var ds = this.Da.ExecuteQuery(sql);

                if (ds.Tables[0].Rows.Count == 1)
                {
                    var sql2 = "update UserInfo set Name = '"+ this.txtFormDataBaseUserName.Text.ToLower() +"', Password = '"+ this.txtFormDataBasePassword.Text +"', Role = '"+ this.cmbFormDataBaseRole.Text +"' where id = '"+ this.txtFormDataBaseID.Text +"';";
                    var rowCount = this.Da.ExecuteDMLQuery(sql2);

                    if (rowCount == 1)
                    {
                        MessageBox.Show("Data has been Updated");
                    }
                    else
                    {
                        MessageBox.Show("Data Upgradation failed");
                    }
                }
                else
                {
                    // Insert
                    string sql1 = "INSERT INTO UserInfo Values ('" + this.txtFormDataBaseID.Text + "', '" + this.txtFormDataBaseUserName.Text + "', '" + this.txtFormDataBasePassword.Text + "', '" + this.cmbFormDataBaseRole.Text + "');";
                    var count = this.Da.ExecuteDMLQuery(sql1);

                    if (count == 1)
                    {
                        MessageBox.Show(this.txtFormDataBaseUserName.Text + " has been added");
                    }
                    else
                    {
                        MessageBox.Show("Data addition failed");
                    }
                }
                this.autoDataBaseCall();
                this.ClearAllTextInsidePnlFormDBAddUpdate();
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
        }

        private void btnFormDBAdd_Click(object sender, EventArgs e) // add members
        {
            try
            {
                string id = this.Da.AutoIdGenerate().ToString();
                this.txtFormDataBaseID.Text = id;

            }
            catch( Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
        }

        private void btnFormDataBaseCancelButton_Click(object sender, EventArgs e)
        {
            this.ClearAllTextInsidePnlFormDBAddUpdate();
        }

        private void btnFormDataBaseDeleteButton_Click(object sender, EventArgs e)
        {
            if (this.dgvUserInfo.SelectedRows.Count < 1)
            {
                MessageBox.Show("Select a row first", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return;
            }

            DialogResult result = MessageBox.Show("Are you sure you want to delete", "Warning", MessageBoxButtons.YesNo, MessageBoxIcon.Warning);
            if (result == DialogResult.No)
                return;

            var id = this.dgvUserInfo.CurrentRow.Cells[0].Value.ToString();
            var tittle = this.dgvUserInfo.CurrentRow.Cells[1].Value.ToString();

            string sql = "delete from UserInfo where ID = '" + id + "';";
            int rowNum = this.Da.ExecuteDMLQuery(sql);

            if (rowNum == 1)
            {
                MessageBox.Show(tittle.ToUpper() + " has been removed");
            }
            else
                MessageBox.Show("Delation has been failed");

            this.autoDataBaseCall();
            this.dgvUserInfo.ClearSelection();
        }

        private void dgvUserInfo_DoubleClick(object sender, EventArgs e) // for update valus
        {
            this.txtFormDataBaseID.Text = this.dgvUserInfo.CurrentRow.Cells[0].Value.ToString();
            this.txtFormDataBaseUserName.Text = this.dgvUserInfo.CurrentRow.Cells[1].Value.ToString();
            this.txtFormDataBasePassword.Text = this.dgvUserInfo.CurrentRow.Cells[2].Value.ToString();
            this.cmbFormDataBaseRole.Text = this.dgvUserInfo.CurrentRow.Cells[3].Value.ToString();
        }
    }
}
