using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace CalculatorProject
{
    public partial class FormVolume : Form
    {
        public FormVolume()
        {
            InitializeComponent();
        }

        private void FormVolume_FormClosed(object sender, FormClosedEventArgs e)
        {
            Application.Exit();
        }
    }
}
