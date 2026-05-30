      ******************************************************************
      *    I#BVVEPN - LAY-OUT DO ARQUIVO DE PROPOSTAS NAO ACATADAS     *
      *               PELA VISA-VALE                                   *
      *    LRECL: 1600                                                 *
      ******************************************************************
       01  REG-PEDCTNAC.
           03  PEDCTNAC-TIPO-REG         PIC 9(001).
7C2511*    03  PEDCTNAC-CNPJ             PIC 9(008).
7C2511     03  PEDCTNAC-CNPJ             PIC X(008).
7C2511*    03  PEDCTNAC-FILIAL           PIC 9(004).
7C2511     03  PEDCTNAC-FILIAL           PIC X(004).
           03  PEDCTNAC-CONTROLE         PIC 9(002).
           03  PEDCTNAC-COD-PRODUTO      PIC 9(003).
           03  PEDCTNAC-NUM-CONTRATO     PIC 9(011).
           03  PEDCTNAC-DATA-REFERENCIA  PIC 9(008).
           03  PEDCTNAC-NUM-REMESSA      PIC 9(006).
           03  PEDCTNAC-RAZAO-SOCIAL     PIC X(040).
           03  PEDCTNAC-FORMA-PGTO       PIC 9(001).
           03  PEDCTNAC-MEIO-PGTO        PIC 9(001).
           03  PEDCTNAC-VLR-LIMITE       PIC 9(013).
           03  PEDCTNAC-QTD-ERROS        PIC 9(003) COMP-3.
           03  PEDCTNAC-TABELA-ERROS.
               05  PEDCTNAC-TAB-ERROS    OCCURS 339.
                   07  PEDCTNAC-TAB-COD-ERRO
                                         PIC X(004).
4SYSJT     03  PEDCTNAC-DT-INCLUSAO      PIC 9(008).
4SYSJT     03  PEDCTNAC-USUARIO          PIC X(009).
4SYSJT     03  PEDCTNAC-AGENCIA          PIC 9(005).
           03  FILLER                    PIC X(122).
