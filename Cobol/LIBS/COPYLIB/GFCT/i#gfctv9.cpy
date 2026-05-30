      *****************************************************************
      * NOME DA INC - I#GFCTV9                                        *
      * DESCRICAO   - ACESSO AO MODULO CORPORATIVO GFCT6006           *
      * TAMANHO     - 564                                             *
      * DATA        - 10.01.2008                                      *
      * RESPONSAVEL - VINICIUS C. MADUREIRA - PROCWORK - GP50         *
      *****************************************************************
      * MANUTENCAO  - INCLUIR FLAG CESTA CELULAR                      *
      * TAMANHO     - 569                                             *
      * DATA        - 28.05.2010                                      *
      * RESPONSAVEL - ANTONIO PAGNOCCA NETO - SPW - GP50              *
      *****************************************************************
       01  GFCTV9-REG.
           03  GFCTV9-ENTRADA.
               05  GFCTV9-AMBIENTE           PIC  X(001).
               05  GFCTV9-AGENCIA            PIC  9(005).
               05  GFCTV9-CONTA              PIC  9(007).
               05  GFCTV9-PONTEIRO           PIC  9(005).
           03  GFCTV9-SAIDA.
               05  GFCTV9-COD-RETORNO        PIC  9(002).
               05  GFCTV9-COD-MENSAGEM       PIC  9(004).
               05  GFCTV9-MENSAGEM           PIC  X(079).
               05  GFCTV9-SQLCA              PIC  X(136).
               05  GFCTV9-DADOS-TARIFA OCCURS 5 TIMES.
                   07 GFCTV9-TARIFA          PIC  9(005).
                   07 GFCTV9-NOME-TARIFA     PIC  X(040).
                   07 GFCTV9-DATA-INICIO     PIC  X(010).
                   07 GFCTV9-DATA-FIM        PIC  X(010).
                   07 GFCTV9-CESTA-CELULAR   PIC  X(001).
