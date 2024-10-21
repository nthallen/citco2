package DoReport;

our %default_msg = (
  To => 'Debra Wunch <dwunch@atmosp.physics.utoronto.ca>,
         Joseph Mendonca <joseph.mendonca@utoronto.ca>,
         Kris Kunz <etluofteclab@gmail.com>,
         TCCON Account <tccon@gps.caltech.edu>',
  Cc => 'Norton Allen <allen@huarp.harvard.edu>,
         Jean-Francois Blavier <jblavier@caesar.jpl.nasa.gov>',
  # To => 'Norton Allen <allen@huarp.harvard.edu>',
  From => 'East Trout Lake IFS11 <tccon.etl@huarp.harvard.edu>',
  Subject => 'etl_ifs11',
  Message => 'stuff',
  Message_ID_Template => 'etl_ifs11-%d-%d-%d@tccon'
);

1;
