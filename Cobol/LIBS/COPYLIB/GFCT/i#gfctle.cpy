      *****************************************************************
      * NOME DA INC - I#GFCTLE                                        *
      * DESCRICAO   - MOVIMENTO ONLINE - PACOTES VIGENTES E FUTUROS   *
      *               COMBO                                           *
      *               COMUNICACAO IMS X WEB - SAIDA              5124 *
      * TAMANHO     - 1010                                            *
      * DATA        - 01.12.2005                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL.: 4152-4352    *
      *****************************************************************
      *
       01  GFCTLE-SAIDA.
           03  GFCTLE-LL                            PIC S9(004) COMP.
           03  GFCTLE-ZZ                            PIC S9(004) COMP.
           03  GFCTLE-TRANSACAO                     PIC  X(009).
           03  GFCTLE-FUNCAO                        PIC  X(008).
           03  GFCTLE-FILTRO.
               05  GFCTLE-FILTRO-AGENCIA            PIC  9(005).
               05  GFCTLE-FILTRO-CONTA              PIC  9(007).
               05  GFCTLE-FILTRO-TIPO-CONTA         PIC  9(001).
               05  GFCTLE-FILTRO-TIPO-PESSOA        PIC  X(001).
               05  GFCTLE-FILTRO-ACESSO             PIC  X(001).
      *            'I' - INCLUSAO
      *            'A' - ALTERACAO
           03  GFCTLE-PONTEIRO.
               05  GFCTLE-PONTEIRO-TARIFA           PIC  9(005).
           03  GFCTLE-QTDE-OCOR                     PIC  9(003).
           03  GFCTLE-QTDE-TOT-REG                  PIC  9(006).
           03  GFCTLE-FUNC-BDSCO                    PIC  X(007).
           03  GFCTLE-ERRO                          PIC  9(001).
           03  GFCTLE-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTLE-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTLE-DESC-MSG-ERRO                 PIC  X(079).
           03  GFCTLE-FIM                           PIC  X(001).
           03  FILLER                               PIC  X(202).
           03  GFCTLE-DADOS.
               05  GFCTLE-LISTA-PACOTE              OCCURS 10.
                   07  GFCTLE-TARIFA-PACOTE         PIC  9(005).
                   07  GFCTLE-DESCR-TARIFA          PIC  X(040).
                   07  GFCTLE-DT-INI-VIG-ADESAO     PIC  X(010).
                   07  GFCTLE-DT-FIM-VIG-ADESAO     PIC  X(010).
                   07  GFCTLE-INCD-TARIFA           PIC  9(001).
