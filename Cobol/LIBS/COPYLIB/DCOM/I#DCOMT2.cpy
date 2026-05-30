      ******************************************************************
      * SISTEMA..: DCOM - I#DCOMT2                                     *
      * TAMANHO..: 121 BYTES                                           *
      * ARQUIVO..: INTERFACE DCOM X RISC                               *
      * OBJETIVO.: ARQUIVO COM MOTIVO DA BAIXA PARA ENVIO AO BACEN     *
      ******************************************************************
       01  T2-REGISTRO.
           05  T2-CENTRO-CUSTO            PIC  X(004).
           05  T2-AGENCIA                 PIC  9(005) COMP-3.
           05  T2-CONTA                   PIC  9(013) COMP-3.
           05  T2-CARTEIRA                PIC  X(003).
           05  T2-CONTRATO                PIC  9(017) COMP-3.
           05  T2-EMPRESA                 PIC  9(005) COMP-3.
           05  T2-CNPJ-NUMERO-CLIENTE     PIC  9(009) COMP-3.
           05  T2-CNPJ-FILIAL-CLIENTE     PIC  9(004) COMP-3.
           05  T2-CNPJ-CONTR-CLIENTE      PIC  9(002) COMP-3.
           05  T2-CNPJ-NUMERO-INCORP      PIC  9(009) COMP-3.
           05  T2-CNPJ-FILIAL-INCORP      PIC  9(004) COMP-3.
           05  T2-CNPJ-CONTR-INCORP       PIC  9(002) COMP-3.
           05  T2-CNPJ-NUMERO-CEES        PIC  9(009) COMP-3.
           05  T2-CNPJ-FILIAL-CEES        PIC  9(004) COMP-3.
           05  T2-CNPJ-CONTR-CEES         PIC  9(002) COMP-3.
           05  T2-VALOR                   PIC  9(015)V9(002) COMP-3.
           05  T2-DATA-CESSAO             PIC  X(010).
           05  T2-NUM-CONTRATO-NOVO       PIC  X(036).
           05  T2-MODAL-NOVA              PIC  9(005) COMP-3.
           05  T2-MOTIVO-BAIXA            PIC  9(004).
