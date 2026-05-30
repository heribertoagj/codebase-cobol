      ******************************************************************
      *    I#BVVEPA - LAY-OUT DO ARQUIVO DE PROPOSTAS ACATADAS PELA    *
      *               VISA-VALE                                        *
      *    LRECL: 200                                                  *
      ******************************************************************
       01  REG-PEDCTAC.
7C2511*    03  PEDCTAC-CNPJ            PIC 9(008).
7C2511     03  PEDCTAC-CNPJ            PIC X(008).
7C2511*    03  PEDCTAC-FILIAL          PIC 9(004).
7C2511     03  PEDCTAC-FILIAL          PIC X(004).
           03  PEDCTAC-CONTROLE        PIC 9(002).
           03  PEDCTAC-COD-PRODUTO     PIC 9(003).
           03  PEDCTAC-NUM-CONTRATO    PIC 9(011).
           03  PEDCTAC-RAZAO-SOCIAL    PIC X(040).
           03  PEDCTAC-FORMA-PGTO      PIC 9(001).
           03  PEDCTAC-MEIO-PGTO       PIC 9(001).
           03  PEDCTAC-VLR-LIMITE      PIC 9(013).
4SYSJT     03  PEDCTAC-FUNCIONARIO     PIC X(009).
4SYSJT     03  PEDCTAC-AGENCIA         PIC 9(005).
           03  FILLER                  PIC X(103).
