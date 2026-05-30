      *****************************************************************
      * NOME DA INC - I#GFCTLD                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - PACOTES VIGENTES E FUTUROS   *
      *               COMBO                                           *
      *               COMUNICACAO IMS X WEB - ENTRADA                 *
      * TAMANHO     - 100                                             *
      * DATA        - 01.12.2005                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL.: 4152-4352    *
      *****************************************************************
      *
       01  GFCTLD-ENTRADA.
           03  GFCTLD-LL                            PIC S9(004) COMP.
           03  GFCTLD-ZZ                            PIC S9(004) COMP.
           03  GFCTLD-TRANSACAO                     PIC  X(009).
           03  GFCTLD-FUNCAO                        PIC  X(008).
           03  GFCTLD-DADOS-RST.
               05  GFCTLD-FILTRO.
                   07  GFCTLD-FILTRO-AGENCIA        PIC  9(005).
                   07  GFCTLD-FILTRO-CONTA          PIC  9(007).
                   07  GFCTLD-FILTRO-TIPO-CONTA     PIC  9(001).
                   07  GFCTLD-FILTRO-TIPO-PESSOA    PIC  X(001).
                   07  GFCTLD-FILTRO-ACESSO         PIC  X(001).
      *                'I' - INCLUSAO
      *                'A' - ALTERACAO
               05  GFCTLD-PONTEIRO.
                   07 GFCTLD-PONTEIRO-TARIFA        PIC  9(005).
               05  GFCTLD-QTDE-OCOR                 PIC  9(003).
               05  GFCTLD-QTDE-TOT-REG              PIC  9(006).
               05  GFCTLD-FUNC-BDSCO                PIC  X(007).
               05  GFCTLD-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(042).
