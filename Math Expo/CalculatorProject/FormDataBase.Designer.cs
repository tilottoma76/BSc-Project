namespace CalculatorProject
{
    partial class Form1DataBase
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Form1DataBase));
            this.panel1 = new System.Windows.Forms.Panel();
            this.dgvUserInfo = new System.Windows.Forms.DataGridView();
            this.ID = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.UserName = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Password = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Role = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.pnlUperPanel = new System.Windows.Forms.Panel();
            this.btnFormDataBaseDeleteButton = new System.Windows.Forms.Button();
            this.pnlFormDBAddUpdate = new System.Windows.Forms.Panel();
            this.btnFormDataBaseCancelButton = new System.Windows.Forms.Button();
            this.btnFormDataBaseSaveButton = new System.Windows.Forms.Button();
            this.txtFormDataBaseUserName = new System.Windows.Forms.TextBox();
            this.label4 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.txtFormDataBasePassword = new System.Windows.Forms.TextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.txtFormDataBaseID = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.panel2 = new System.Windows.Forms.Panel();
            this.panel3 = new System.Windows.Forms.Panel();
            this.btnFormDbSearch = new System.Windows.Forms.PictureBox();
            this.txtFormDbSearch = new System.Windows.Forms.TextBox();
            this.btnFormDBAdd = new System.Windows.Forms.Button();
            this.cmbFormDataBaseRole = new System.Windows.Forms.ComboBox();
            this.panel1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.dgvUserInfo)).BeginInit();
            this.pnlUperPanel.SuspendLayout();
            this.pnlFormDBAddUpdate.SuspendLayout();
            this.panel2.SuspendLayout();
            this.panel3.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.btnFormDbSearch)).BeginInit();
            this.SuspendLayout();
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.dgvUserInfo);
            this.panel1.Controls.Add(this.pnlUperPanel);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panel1.Location = new System.Drawing.Point(0, 0);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(774, 504);
            this.panel1.TabIndex = 0;
            // 
            // dgvUserInfo
            // 
            this.dgvUserInfo.AllowUserToAddRows = false;
            this.dgvUserInfo.AllowUserToDeleteRows = false;
            this.dgvUserInfo.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dgvUserInfo.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.ID,
            this.UserName,
            this.Password,
            this.Role});
            this.dgvUserInfo.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.dgvUserInfo.Location = new System.Drawing.Point(0, 241);
            this.dgvUserInfo.Name = "dgvUserInfo";
            this.dgvUserInfo.ReadOnly = true;
            this.dgvUserInfo.RowHeadersWidth = 51;
            this.dgvUserInfo.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect;
            this.dgvUserInfo.Size = new System.Drawing.Size(774, 263);
            this.dgvUserInfo.TabIndex = 2;
            this.dgvUserInfo.DoubleClick += new System.EventHandler(this.dgvUserInfo_DoubleClick);
            // 
            // ID
            // 
            this.ID.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.ID.DataPropertyName = "ID";
            this.ID.HeaderText = "ID";
            this.ID.MinimumWidth = 6;
            this.ID.Name = "ID";
            this.ID.ReadOnly = true;
            // 
            // UserName
            // 
            this.UserName.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.UserName.DataPropertyName = "Name";
            this.UserName.HeaderText = "User Name";
            this.UserName.MinimumWidth = 6;
            this.UserName.Name = "UserName";
            this.UserName.ReadOnly = true;
            // 
            // Password
            // 
            this.Password.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.Password.DataPropertyName = "Password";
            this.Password.HeaderText = "User Password";
            this.Password.MinimumWidth = 6;
            this.Password.Name = "Password";
            this.Password.ReadOnly = true;
            // 
            // Role
            // 
            this.Role.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill;
            this.Role.DataPropertyName = "Role";
            this.Role.HeaderText = "User Role";
            this.Role.MinimumWidth = 6;
            this.Role.Name = "Role";
            this.Role.ReadOnly = true;
            // 
            // pnlUperPanel
            // 
            this.pnlUperPanel.Controls.Add(this.btnFormDBAdd);
            this.pnlUperPanel.Controls.Add(this.btnFormDataBaseDeleteButton);
            this.pnlUperPanel.Controls.Add(this.pnlFormDBAddUpdate);
            this.pnlUperPanel.Controls.Add(this.panel2);
            this.pnlUperPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pnlUperPanel.Location = new System.Drawing.Point(0, 0);
            this.pnlUperPanel.Name = "pnlUperPanel";
            this.pnlUperPanel.Size = new System.Drawing.Size(774, 504);
            this.pnlUperPanel.TabIndex = 1;
            // 
            // btnFormDataBaseDeleteButton
            // 
            this.btnFormDataBaseDeleteButton.FlatAppearance.BorderColor = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.btnFormDataBaseDeleteButton.FlatAppearance.BorderSize = 2;
            this.btnFormDataBaseDeleteButton.FlatAppearance.MouseDownBackColor = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(192)))), ((int)(((byte)(192)))));
            this.btnFormDataBaseDeleteButton.FlatAppearance.MouseOverBackColor = System.Drawing.Color.FromArgb(((int)(((byte)(192)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))));
            this.btnFormDataBaseDeleteButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.btnFormDataBaseDeleteButton.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnFormDataBaseDeleteButton.ForeColor = System.Drawing.Color.Black;
            this.btnFormDataBaseDeleteButton.Location = new System.Drawing.Point(462, 17);
            this.btnFormDataBaseDeleteButton.Name = "btnFormDataBaseDeleteButton";
            this.btnFormDataBaseDeleteButton.Size = new System.Drawing.Size(83, 68);
            this.btnFormDataBaseDeleteButton.TabIndex = 22;
            this.btnFormDataBaseDeleteButton.Text = "Delete";
            this.btnFormDataBaseDeleteButton.UseVisualStyleBackColor = true;
            this.btnFormDataBaseDeleteButton.Click += new System.EventHandler(this.btnFormDataBaseDeleteButton_Click);
            // 
            // pnlFormDBAddUpdate
            // 
            this.pnlFormDBAddUpdate.BackColor = System.Drawing.SystemColors.InactiveCaption;
            this.pnlFormDBAddUpdate.Controls.Add(this.cmbFormDataBaseRole);
            this.pnlFormDBAddUpdate.Controls.Add(this.btnFormDataBaseCancelButton);
            this.pnlFormDBAddUpdate.Controls.Add(this.btnFormDataBaseSaveButton);
            this.pnlFormDBAddUpdate.Controls.Add(this.txtFormDataBaseUserName);
            this.pnlFormDBAddUpdate.Controls.Add(this.label4);
            this.pnlFormDBAddUpdate.Controls.Add(this.label3);
            this.pnlFormDBAddUpdate.Controls.Add(this.txtFormDataBasePassword);
            this.pnlFormDBAddUpdate.Controls.Add(this.label2);
            this.pnlFormDBAddUpdate.Controls.Add(this.txtFormDataBaseID);
            this.pnlFormDBAddUpdate.Controls.Add(this.label1);
            this.pnlFormDBAddUpdate.Location = new System.Drawing.Point(2, 2);
            this.pnlFormDBAddUpdate.Margin = new System.Windows.Forms.Padding(2);
            this.pnlFormDBAddUpdate.Name = "pnlFormDBAddUpdate";
            this.pnlFormDBAddUpdate.Size = new System.Drawing.Size(248, 181);
            this.pnlFormDBAddUpdate.TabIndex = 1;
            // 
            // btnFormDataBaseCancelButton
            // 
            this.btnFormDataBaseCancelButton.FlatAppearance.BorderColor = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.btnFormDataBaseCancelButton.FlatAppearance.BorderSize = 2;
            this.btnFormDataBaseCancelButton.FlatAppearance.MouseDownBackColor = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(192)))), ((int)(((byte)(192)))));
            this.btnFormDataBaseCancelButton.FlatAppearance.MouseOverBackColor = System.Drawing.Color.FromArgb(((int)(((byte)(192)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))));
            this.btnFormDataBaseCancelButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.btnFormDataBaseCancelButton.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnFormDataBaseCancelButton.ForeColor = System.Drawing.Color.Black;
            this.btnFormDataBaseCancelButton.Location = new System.Drawing.Point(135, 138);
            this.btnFormDataBaseCancelButton.Name = "btnFormDataBaseCancelButton";
            this.btnFormDataBaseCancelButton.Size = new System.Drawing.Size(64, 32);
            this.btnFormDataBaseCancelButton.TabIndex = 21;
            this.btnFormDataBaseCancelButton.Text = "Clear";
            this.btnFormDataBaseCancelButton.UseVisualStyleBackColor = true;
            this.btnFormDataBaseCancelButton.Click += new System.EventHandler(this.btnFormDataBaseCancelButton_Click);
            // 
            // btnFormDataBaseSaveButton
            // 
            this.btnFormDataBaseSaveButton.FlatAppearance.BorderColor = System.Drawing.Color.Lime;
            this.btnFormDataBaseSaveButton.FlatAppearance.BorderSize = 2;
            this.btnFormDataBaseSaveButton.FlatAppearance.MouseDownBackColor = System.Drawing.Color.FromArgb(((int)(((byte)(192)))), ((int)(((byte)(255)))), ((int)(((byte)(192)))));
            this.btnFormDataBaseSaveButton.FlatAppearance.MouseOverBackColor = System.Drawing.Color.Green;
            this.btnFormDataBaseSaveButton.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.btnFormDataBaseSaveButton.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnFormDataBaseSaveButton.ForeColor = System.Drawing.Color.Black;
            this.btnFormDataBaseSaveButton.Location = new System.Drawing.Point(60, 138);
            this.btnFormDataBaseSaveButton.Name = "btnFormDataBaseSaveButton";
            this.btnFormDataBaseSaveButton.Size = new System.Drawing.Size(59, 32);
            this.btnFormDataBaseSaveButton.TabIndex = 20;
            this.btnFormDataBaseSaveButton.Text = "Save";
            this.btnFormDataBaseSaveButton.UseVisualStyleBackColor = true;
            this.btnFormDataBaseSaveButton.Click += new System.EventHandler(this.btnFormDataBaseSaveButton_Click);
            // 
            // txtFormDataBaseUserName
            // 
            this.txtFormDataBaseUserName.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtFormDataBaseUserName.Location = new System.Drawing.Point(100, 42);
            this.txtFormDataBaseUserName.Margin = new System.Windows.Forms.Padding(2);
            this.txtFormDataBaseUserName.Name = "txtFormDataBaseUserName";
            this.txtFormDataBaseUserName.Size = new System.Drawing.Size(133, 21);
            this.txtFormDataBaseUserName.TabIndex = 7;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label4.Location = new System.Drawing.Point(10, 47);
            this.label4.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(91, 15);
            this.label4.TabIndex = 6;
            this.label4.Text = "User Name : ";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label3.Location = new System.Drawing.Point(10, 112);
            this.label3.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(49, 15);
            this.label3.TabIndex = 4;
            this.label3.Text = "Role : ";
            // 
            // txtFormDataBasePassword
            // 
            this.txtFormDataBasePassword.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtFormDataBasePassword.Location = new System.Drawing.Point(100, 75);
            this.txtFormDataBasePassword.Margin = new System.Windows.Forms.Padding(2);
            this.txtFormDataBasePassword.Name = "txtFormDataBasePassword";
            this.txtFormDataBasePassword.Size = new System.Drawing.Size(133, 21);
            this.txtFormDataBasePassword.TabIndex = 3;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label2.Location = new System.Drawing.Point(10, 80);
            this.label2.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(81, 15);
            this.label2.TabIndex = 2;
            this.label2.Text = "Password : ";
            // 
            // txtFormDataBaseID
            // 
            this.txtFormDataBaseID.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.txtFormDataBaseID.Location = new System.Drawing.Point(100, 10);
            this.txtFormDataBaseID.Margin = new System.Windows.Forms.Padding(2);
            this.txtFormDataBaseID.Name = "txtFormDataBaseID";
            this.txtFormDataBaseID.ReadOnly = true;
            this.txtFormDataBaseID.Size = new System.Drawing.Size(133, 21);
            this.txtFormDataBaseID.TabIndex = 1;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.Location = new System.Drawing.Point(10, 15);
            this.label1.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(33, 15);
            this.label1.TabIndex = 0;
            this.label1.Text = "ID : ";
            // 
            // panel2
            // 
            this.panel2.Controls.Add(this.panel3);
            this.panel2.Location = new System.Drawing.Point(0, 186);
            this.panel2.Name = "panel2";
            this.panel2.Size = new System.Drawing.Size(774, 53);
            this.panel2.TabIndex = 0;
            // 
            // panel3
            // 
            this.panel3.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(17)))), ((int)(((byte)(53)))), ((int)(((byte)(71)))));
            this.panel3.Controls.Add(this.btnFormDbSearch);
            this.panel3.Controls.Add(this.txtFormDbSearch);
            this.panel3.Location = new System.Drawing.Point(553, 5);
            this.panel3.Name = "panel3";
            this.panel3.Size = new System.Drawing.Size(218, 45);
            this.panel3.TabIndex = 2;
            // 
            // btnFormDbSearch
            // 
            this.btnFormDbSearch.Image = ((System.Drawing.Image)(resources.GetObject("btnFormDbSearch.Image")));
            this.btnFormDbSearch.Location = new System.Drawing.Point(181, 3);
            this.btnFormDbSearch.Name = "btnFormDbSearch";
            this.btnFormDbSearch.Size = new System.Drawing.Size(31, 39);
            this.btnFormDbSearch.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom;
            this.btnFormDbSearch.TabIndex = 1;
            this.btnFormDbSearch.TabStop = false;
            this.btnFormDbSearch.Click += new System.EventHandler(this.btnFormDbSearch_Click);
            // 
            // txtFormDbSearch
            // 
            this.txtFormDbSearch.Location = new System.Drawing.Point(3, 13);
            this.txtFormDbSearch.Name = "txtFormDbSearch";
            this.txtFormDbSearch.Size = new System.Drawing.Size(172, 20);
            this.txtFormDbSearch.TabIndex = 0;
            // 
            // btnFormDBAdd
            // 
            this.btnFormDBAdd.FlatAppearance.BorderColor = System.Drawing.Color.Lime;
            this.btnFormDBAdd.FlatAppearance.BorderSize = 2;
            this.btnFormDBAdd.FlatAppearance.MouseDownBackColor = System.Drawing.Color.FromArgb(((int)(((byte)(192)))), ((int)(((byte)(255)))), ((int)(((byte)(192)))));
            this.btnFormDBAdd.FlatAppearance.MouseOverBackColor = System.Drawing.Color.Green;
            this.btnFormDBAdd.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.btnFormDBAdd.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnFormDBAdd.ForeColor = System.Drawing.Color.Black;
            this.btnFormDBAdd.Location = new System.Drawing.Point(357, 17);
            this.btnFormDBAdd.Name = "btnFormDBAdd";
            this.btnFormDBAdd.Size = new System.Drawing.Size(82, 68);
            this.btnFormDBAdd.TabIndex = 23;
            this.btnFormDBAdd.Text = "Add";
            this.btnFormDBAdd.UseVisualStyleBackColor = true;
            this.btnFormDBAdd.Click += new System.EventHandler(this.btnFormDBAdd_Click);
            // 
            // cmbFormDataBaseRole
            // 
            this.cmbFormDataBaseRole.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbFormDataBaseRole.FormattingEnabled = true;
            this.cmbFormDataBaseRole.Items.AddRange(new object[] {
            "user",
            "admin"});
            this.cmbFormDataBaseRole.Location = new System.Drawing.Point(100, 106);
            this.cmbFormDataBaseRole.Name = "cmbFormDataBaseRole";
            this.cmbFormDataBaseRole.Size = new System.Drawing.Size(133, 21);
            this.cmbFormDataBaseRole.TabIndex = 22;
            // 
            // Form1DataBase
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(774, 504);
            this.Controls.Add(this.panel1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None;
            this.Name = "Form1DataBase";
            this.Text = "FormDataBase";
            this.Load += new System.EventHandler(this.Form1DataBase_Load);
            this.panel1.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.dgvUserInfo)).EndInit();
            this.pnlUperPanel.ResumeLayout(false);
            this.pnlFormDBAddUpdate.ResumeLayout(false);
            this.pnlFormDBAddUpdate.PerformLayout();
            this.panel2.ResumeLayout(false);
            this.panel3.ResumeLayout(false);
            this.panel3.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.btnFormDbSearch)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Panel pnlUperPanel;
        private System.Windows.Forms.DataGridView dgvUserInfo;
        private System.Windows.Forms.DataGridViewTextBoxColumn ID;
        private System.Windows.Forms.DataGridViewTextBoxColumn UserName;
        private System.Windows.Forms.DataGridViewTextBoxColumn Password;
        private System.Windows.Forms.DataGridViewTextBoxColumn Role;
        private System.Windows.Forms.Panel panel2;
        private System.Windows.Forms.Panel panel3;
        private System.Windows.Forms.PictureBox btnFormDbSearch;
        private System.Windows.Forms.TextBox txtFormDbSearch;
        private System.Windows.Forms.Panel pnlFormDBAddUpdate;
        private System.Windows.Forms.TextBox txtFormDataBasePassword;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox txtFormDataBaseID;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox txtFormDataBaseUserName;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Button btnFormDataBaseSaveButton;
        private System.Windows.Forms.Button btnFormDataBaseCancelButton;
        private System.Windows.Forms.Button btnFormDataBaseDeleteButton;
        private System.Windows.Forms.Button btnFormDBAdd;
        private System.Windows.Forms.ComboBox cmbFormDataBaseRole;
    }
}