      *****************************************************************
      * NOME DA INC - I#GFCT62                                        *
      * DESCRICAO   - GESTAO - DEMONSTRATIVO DE EVENTOS DO CLIENTE    *
      *               CONSULTA                                        *
      *               COMUNICACAO WEB X IMS - ENTRADA                 *
      * TAMANHO     - 250                                             *
      * DATA        - 26.10.2007                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 30689902 R 227 *
      *****************************************************************
      *
       01  GFCT62-ENTRADA.
           03  GFCT62-LL                            PIC S9(004) COMP.
           03  GFCT62-ZZ                            PIC S9(004) COMP.
           03  GFCT62-TRANSACAO                     PIC  X(009).
           03  GFCT62-FUNCAO                        PIC  X(008).
           03  GFCT62-DADOS-RST.
               05  GFCT62-FILTRO.
                   07  GFCT62-AGENCIA               PIC  9(005).
                   07  GFCT62-CONTA                 PIC  9(013).
                   07  GFCT62-PERIODO-INI           PIC  X(010).
                   07  GFCT62-PERIODO-FIM           PIC  X(010).
                   07  GFCT62-COD-CJTO-SERVC        PIC  9(003).
                   07  GFCT62-COD-TARIFA            PIC  9(005).
               05  GFCT62-PONTEIRO                  PIC  9(003).
               05  GFCT62-OCOR-ATUAL                PIC  9(007).
               05  GFCT62-OCOR-TOTAL                PIC  9(007).
               05  GFCT62-DETALHES                  PIC  X(001).
               05  GFCT62-CONTINUA                  PIC  X(001).
               05  GFCT62-QTDE-OCOR                 PIC  9(003).
               05  GFCT62-QTDE-TOT-REG              PIC  9(006).
               05  GFCT62-DEPTO-BDSCO               PIC  9(005).
               05  GFCT62-FUNC-BDSCO                PIC  X(007).
               05  GFCT62-FIM                       PIC  X(001).
               05  FILLER                           PIC  X(142).
