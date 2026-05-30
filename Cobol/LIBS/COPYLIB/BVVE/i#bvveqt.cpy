      ******************************************************************
      *    I#BVVETQ - LAY-OUT DO ARQUIVO DE TARIFAS POR QUANTIDADE     *
      *               DE CARTOES                                       *
      *    LRECL: 100                                                  *
      ******************************************************************
       01  PARM-TARIFA-QTDE.
           03  PARM-TFQT-CH.
               05  PARM-TFQT-CH-CODIGO-PROD  PIC 9(03).
               05  PARM-TFQT-CH-CODIGO-TARI  PIC 9(03).
               05  PARM-TFQT-CH-FORMA-PAGTO  PIC X(01).
               05  PARM-TFQT-CH-QTD-CARTAO-ATE
                                             PIC 9(06).
4SYSJT         05  PARM-TFQT-CH-PRAZO-PAGTO  PIC 9(03).
.              05  PARM-TFQT-CH-FATMT-PREVT  PIC 9(09)V99.
.          03  PARM-TFQT-TARIF-OBRIG         PIC X(01).
4SYSJT     03  PARM-TFQT-TARIF-FORMT         PIC X(01).
           03  PARM-TFQT-VALOR               PIC 9(03)V99.
           03  PARM-TFQT-DT-INCLUSAO         PIC 9(08).
           03  PARM-TFQT-USUARIO             PIC X(09).
           03  PARM-TFQT-FILLER              PIC X(49).
