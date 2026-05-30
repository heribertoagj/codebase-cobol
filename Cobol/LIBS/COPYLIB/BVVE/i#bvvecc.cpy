      ******************************************************************
      *    I#BVVECC - LAY-OUT DO ARQUIVO DE ENVIO / RETORNO A REDECARD *
      *               REFERENTE A CONFIRMACAO DE DOMICILIO ENVIADO     *
      *    LRECL: 256                                                  *
      ******************************************************************
       01  BVVECC-HEADER.
           03  BVVECC-TIPO-REG          PIC 9(003).
           03  BVVECC-BANCO             PIC 9(005).
           03  BVVECC-EC                PIC 9(010).
           03  BVVECC-SEQUENCIA         PIC 9(008).
           03  BVVECC-DT-PROC           PIC 9(008).
           03  BVVECC-HR-PROC           PIC 9(006).
           03  FILLER                   PIC X(216).

       01  BVVECC-DETALHE REDEFINES     BVVECC-HEADER.
           03  BVVECC-TIPO-REG          PIC 9(003).
           03  BVVECC-BANCO             PIC 9(005).
           03  BVVECC-EC                PIC 9(010).
           03  BVVECC-TIPO-PESSOA       PIC X(001).
           03  BVVECC-CPF-CNPJ          PIC 9(015).
           03  BVVECC-STATUS-CPF-CNPJ   PIC 9(002).
           03  BVVECC-AGENCIA           PIC 9(005).
           03  BVVECC-DV-AGENCIA        PIC X(001).
           03  BVVECC-STATUS-AGENCIA    PIC 9(002).
           03  BVVECC-CONTA             PIC X(014).
           03  BVVECC-DV-CONTA          PIC X(002).
           03  BVVECC-STATUS-CONTA      PIC 9(002).
           03  BVVECC-DAC               PIC X(001).
           03  BVVECC-ESTAGIO-VALID     PIC 9(002).
           03  BVVECC-TIPO-CONTA        PIC 9(001).
           03  BVVECC-RAZAO-SOCIAL      PIC X(060).
           03  BVVECC-CEP               PIC 9(008).
           03  BVVECC-TIPO-DOMIC        PIC X(002).
           03  BVVECC-DT-ENVIO          PIC 9(008).
           03  BVVECC-DT-RETORNO        PIC 9(008).
           03  FILLER                   PIC X(104).

       01  BVVECC-TRAILLER REDEFINES    BVVECC-HEADER.
           03  BVVECC-TIPO-REG          PIC 9(003).
           03  BVVECC-BANCO             PIC 9(005).
           03  BVVECC-EC                PIC 9(010).
           03  BVVECC-QTD-REG-DET       PIC 9(008).
           03  FILLER                   PIC X(230).
