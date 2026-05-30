      *****************************************************************
      * NOME DA INC - I#GFCT63                                        *
      * DESCRICAO   - GESTAO - DEMONSTRATIVO DE EVENTOS DO CLIENTE    *
      *               CONSULTA                                        *
      *               COMUNICACAO WEB X IMS - SAIDA                   *
      * TAMANHO     - 1010                                            *
      * DATA        - 26.10.2007                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 30689902 R 227 *
      *****************************************************************
      *
       01  GFCT63-SAIDA.
           03  GFCT63-LL                            PIC S9(004) COMP.
           03  GFCT63-ZZ                            PIC S9(004) COMP.
           03  GFCT63-TRANSACAO                     PIC  X(009).
           03  GFCT63-FUNCAO                        PIC  X(008).
           03  GFCT63-FILTRO.
               05  GFCT63-AGENCIA                   PIC  9(005).
               05  GFCT63-CONTA                     PIC  9(013).
               05  GFCT63-PERIODO-INI               PIC  X(010).
               05  GFCT63-PERIODO-FIM               PIC  X(010).
               05  GFCT63-COD-CJTO-SERVC            PIC  9(003).
               05  GFCT63-COD-TARIFA                PIC  9(005).
           03  GFCT63-PONTEIRO                      PIC  9(003).
           03  GFCT63-OCOR-ATUAL                    PIC  9(007).
           03  GFCT63-OCOR-TOTAL                    PIC  9(007).
           03  GFCT63-DETALHES                      PIC  X(001).
           03  GFCT63-CONTINUA                      PIC  X(001).
           03  GFCT63-QTDE-OCOR                     PIC  9(003).
           03  GFCT63-QTDE-TOT-REG                  PIC  9(006).
           03  GFCT63-DEPTO-BDSCO                   PIC  9(005).
           03  GFCT63-FUNC-BDSCO                    PIC  X(007).
           03  GFCT63-FIM                           PIC  X(001).
           03  GFCT63-ERRO                          PIC  9(001).
           03  GFCT63-COD-MSG-ERRO                  PIC  9(007).
           03  GFCT63-COD-SQL-ERRO                  PIC  9(003).
           03  GFCT63-DESC-MSG-ERRO                 PIC  X(079).
           03  FILLER                               PIC  X(037).
           03  GFCT63-DADOS.
               05  FILLER                           OCCURS 5.
                   07  GFCT63-SEL                   PIC  X(001).
                   07  GFCT63-DT-EVENTO             PIC  X(010).
                   07  GFCT63-CD-TARIFA             PIC  9(005).
                   07  GFCT63-NOME-TARIFA           PIC  X(030).
                   07  GFCT63-DEB-PARCIAL           PIC  X(001).
      *---             'S' - TEM DEBITO PARCIAL
      *---             'N' - NAO TEM DEBITO PARCIAL
                   07  GFCT63-CONS-IMPED-RESTR      PIC  X(001).
      *---             'S' - TEM CONSULTA DE IMPEDIMENTO/RESTRICAO
      *---             'N' - NAO TEM CONSULTA DE IMPEDIMENTO/RESTRICAO
                   07  GFCT63-PARCELAMENTO-COBR     PIC  X(001).
      *---             'S' - TEM PARCELAMENTO NA COBRANCA DA TARIFA
      *---             'N' - NAO TEM PARCELAMENTO NA COBRANCA DA TARIFA
                   07  GFCT63-QT-EVENTOS            PIC  9(006).
                   07  GFCT63-VL-POTENCIAL          PIC  9(009)V99.
                   07  GFCT63-VL-CALCULADO          PIC  9(010)V99.
                   07  GFCT63-VL-RECEBIDO           PIC  9(010)V99.
                   07  GFCT63-VL-RECEBER            PIC  9(010)V99.
                   07  GFCT63-VL-ESTORNADO          PIC  9(010)V99.
                   07  GFCT63-VL-PERDA              PIC  9(008)V99.
                   07  GFCT63-ROTINA                PIC  X(004).
                   07  GFCT63-DT-MOVIMENTO          PIC  X(010).
                   07  GFCT63-ARQUIVO               PIC  9(002).
                   07  GFCT63-SEQ-ARQUIVO           PIC  9(011).
                   07  GFCT63-SEQ-TELA              PIC  9(004).
