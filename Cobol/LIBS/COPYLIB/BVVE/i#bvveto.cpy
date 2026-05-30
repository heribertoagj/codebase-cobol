      *================================================================*
      * I#BVVETO - TRAVA AUTOMATICA                                    *
      *            LOG DO DCOM PARA ENVIO DE TRAVA AUTOMATICA          *
      *            SELECAO EFETUADA NA BASE DB2 BVVEB029               *
      *----------------------------------------------------------------*
      * TAMANHO: 110                                                   *
      *================================================================*
       01  BVVETO-REGISTRO.
           05 BVVETO-AGENCIA               PIC  9(005).
           05 BVVETO-CONTA                 PIC  9(007).
           05 BVVETO-PRODUTO               PIC  9(005).
           05 BVVETO-HATULZ                PIC  X(026).
           05 BVVETO-BAND-CIP              PIC  X(003).
           05 BVVETO-CNPJ-CPF              PIC  9(009).
           05 BVVETO-FILIAL                PIC  9(004).
      *       ZEROS PARA CPF
           05 BVVETO-CTRL                  PIC  9(002).
           05 BVVETO-TP-PSSOA              PIC  X(001).
      *       F = PESSOA FISICA
           05 BVVETO-BAND-BVVE             PIC  9(003).
           05 BVVETO-PRAZO-DIAS            PIC  9(005).
           05 BVVETO-VLR-DEB               PIC  9(013)V99.
           05 FILLER                       PIC  X(025).
