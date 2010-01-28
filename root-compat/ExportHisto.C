void ExportHisto()
{
  TH1F *histo = new TH1F("histo", "My histogram title", 100, -10.0, 10.0);
  for(int i = 0; i < 10000; ++i) histo->Fill(gRandom->Gaus());
  GenerateLispCode(histo);
}

void GenerateLispCode(TH1 *h)
{
  cout <<"(defparameter *histo1d-" << h->GetName() << "*" << endl;
  cout <<"(create-histogram-from-binning :name \"" << h->GetName() << "\"" << endl;
  cout <<":title \"" << h->GetTitle() << "\"" << endl;
  cout <<":nbins " << h->GetNbinsX() << endl;
  cout <<":xmin " << h->GetBinContent(1) << endl;
  cout <<":xmax " << h->GetBinContent(h->GetNbinsX()) << endl;
  cout <<":binning (list " << endl;
  for(int i = 1; i <= h->GetNbinsX(); ++i) {
    cout <<"(make-instance 'cl-asy:bin1d :xmin " << h->GetBinLowEdge(i) << endl;
    cout <<":xmax " << (h->GetBinLowEdge(i) + h->GetBinWidth(i)) << endl;
    cout <<":content "<< h->GetBinContent(i) << ")" << endl;
  }
  cout <<") ;; Close the list of bins" << endl;
  cout <<") ;; Close create-histogram-from-binning" << endl;
  cout <<") ;; Close the defparameter expression" << endl;
}
