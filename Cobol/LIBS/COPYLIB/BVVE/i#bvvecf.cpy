      ******************************************************************
      *    I#BVVECF - LAY-OUT DO ARQUIVO DE ENVIO / RETORNO A REDECARD *
      *               REFERENTE A CONFIRMACAO DE DOMICILIO ENVIADO,    *
      *               CONTENDO TODOS OS ERROS FISICOS ENCONTRADOS      *
      *    LRECL: 300                                                  *
      ******************************************************************
       01  BVVECF-REG.
           03  BVVECF-HEADER.
               05  BVVECF-TIPO-REG       PIC X(003).
               05  BVVECF-BANCO          PIC X(005).
               05  BVVECF-EC             PIC X(010).
               05  BVVECF-SEQUENCIA      PIC X(008).
               05  BVVECF-DT-PROC        PIC X(008).
               05  BVVECF-HR-PROC        PIC X(006).
               05  FILLER                PIC X(216).

           03  BVVECF-DETALHE REDEFINES  BVVECF-HEADER.
               05  FILLER                PIC X(018).
               05  BVVECF-TIPO-PESSOA    PIC X(001).
               05  BVVECF-CPF-CNPJ       PIC X(015).
               05  BVVECF-STATUS-CPFCNPJ PIC X(002).
               05  BVVECF-AGENCIA        PIC X(005).
               05  BVVECF-DV-AGENCIA     PIC X(001).
               05  BVVECF-STATUS-AGENCIA PIC X(002).
               05  BVVECF-CONTA          PIC X(014).
               05  BVVECF-DV-CONTA       PIC X(002).
               05  BVVECF-STATUS-CONTA   PIC X(002).
               05  BVVECF-DAC            PIC X(001).
               05  BVVECF-ESTAGIO-VALID  PIC X(002).
               05  BVVECF-TIPO-CONTA     PIC X(001).
               05  BVVECF-RAZAO-SOCIAL   PIC X(060).
               05  BVVECF-CEP            PIC X(008).
               05  BVVECF-TIPO-DOMIC     PIC X(002).
               05  BVVECF-DT-ENVIO       PIC X(008).
               05  BVVECF-DT-RETORNO     PIC X(008).
               05  FILLER                PIC X(104).

           03  BVVECF-TRAILLER REDEFINES BVVECF-HEADER.
               05  FILLER                PIC X(018).
               05  BVVECF-QTD-REG        PIC X(008).
               05  FILLER                PIC X(230).

           03  BVVECF-TAB-ERRO           PIC 9(001) OCCURS 24.
           03  BVVECF-QTDE-ERROS         PIC 9(003) COMP-3.
           03  FILLER                    PIC X(018).
