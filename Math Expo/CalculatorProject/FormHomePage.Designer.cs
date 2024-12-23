namespace CalculatorProject
{
    partial class FormHomePage
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FormHomePage));
            this.pnlForm1CenterDisplay = new System.Windows.Forms.Panel();
            this.pnlWelcomePicture = new System.Windows.Forms.Panel();
            this.pnlForm1CenterDisplay.SuspendLayout();
            this.SuspendLayout();
            // 
            // pnlForm1CenterDisplay
            // 
            this.pnlForm1CenterDisplay.BackColor = System.Drawing.Color.WhiteSmoke;
            this.pnlForm1CenterDisplay.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("pnlForm1CenterDisplay.BackgroundImage")));
            this.pnlForm1CenterDisplay.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch;
            this.pnlForm1CenterDisplay.Controls.Add(this.pnlWelcomePicture);
            this.pnlForm1CenterDisplay.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pnlForm1CenterDisplay.Location = new System.Drawing.Point(0, 0);
            this.pnlForm1CenterDisplay.Name = "pnlForm1CenterDisplay";
            this.pnlForm1CenterDisplay.Size = new System.Drawing.Size(774, 504);
            this.pnlForm1CenterDisplay.TabIndex = 10;
            // 
            // pnlWelcomePicture
            // 
            this.pnlWelcomePicture.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("pnlWelcomePicture.BackgroundImage")));
            this.pnlWelcomePicture.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch;
            this.pnlWelcomePicture.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.pnlWelcomePicture.Location = new System.Drawing.Point(12, 41);
            this.pnlWelcomePicture.Name = "pnlWelcomePicture";
            this.pnlWelcomePicture.Size = new System.Drawing.Size(322, 429);
            this.pnlWelcomePicture.TabIndex = 0;
            // 
            // FormHomePage
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(774, 504);
            this.Controls.Add(this.pnlForm1CenterDisplay);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None;
            this.Name = "FormHomePage";
            this.Text = "FormHomePage";
            this.pnlForm1CenterDisplay.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Panel pnlForm1CenterDisplay;
        private System.Windows.Forms.Panel pnlWelcomePicture;
    }
}