﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Xamarin.Forms;
using Xamarin.Forms.Xaml;

namespace FitMetrics.Views
{
    [XamlCompilation(XamlCompilationOptions.Compile)]
    public partial class Geodude : ContentPage
    {
        public Geodude()
        {
            InitializeComponent();
        }
        public static async Task NavigateTo()
        {
            Console.Write("I'm trying to navigate here");
            await Shell.Current.GoToAsync($"{nameof(Geodude)}");
        }
    }

}