      *****************************************************************
      * NOME DA INC - I#GFCTNE                                        *
      * DESCRICAO   - FLEXZ PARA AGRUPAMENTO - LISTA CANCEL PEDIDOS   *
      *               COMUNICACAO WEB X IMS - SAIDA - GFCT0420        *
      * TAMANHO     - 1010                                            *
      * GFCTNE-LL   - 1010                                            *
      * DATA        - 17.03.2006                                      *
      * RESPONSAVEL - VALERIA - PROCWORK                              *
      *****************************************************************
      *
       01  GFCTNE-SAIDA.
           03  GFCTNE-LL                            PIC S9(004) COMP.
           03  GFCTNE-ZZ                            PIC S9(004) COMP.
           03  GFCTNE-TRANSACAO                     PIC  X(009).
           03  GFCTNE-FUNCAO                        PIC  X(008).
           03  GFCTNE-DADOS-RST.
               05  GFCTNE-QTDE-OCOR                 PIC  9(007).
               05  GFCTNE-QTDE-TOT-REG              PIC  9(009).
               05  GFCTNE-FUNC-BDSCO                PIC  X(007).
               05  GFCTNE-FILTRO.
                   07  GFCTNE-AGRUPMTO-FIL          PIC  9(003).
                   07  GFCTNE-TARIFA-FIL            PIC  9(005).
               05  GFCTNE-PONTEIRO.
                   07  GFCTNE-AGRUPMTO-PONT         PIC  9(003).
                   07  GFCTNE-TARIFA-PONT           PIC  9(005).
                   07  GFCTNE-TIMESTAMP-PONT        PIC  X(026).
                   07  GFCTNE-SEQUENCIA-PONT        PIC  9(002).
                   07  GFCTNE-DATA-PONT             PIC  X(010).
           03  GFCTNE-ERRO                          PIC  9(001).
           03  GFCTNE-COD-MSG-ERRO                  PIC  9(007).
           03  GFCTNE-COD-SQL-ERRO                  PIC  9(003).
           03  GFCTNE-DESC-MSG-ERRO                 PIC  X(080).
           03  GFCTNE-FIM                           PIC  X(001).
           03  GFCTNE-FILLER                        PIC  X(056).
           03  GFCTNE-REG OCCURS 4.
ST2506*            07  GFCTNE-CGC-CPF               PIC  9(009).
ST2506*            07  GFCTNE-FILIAL                PIC  9(005).
ST2506             07  GFCTNE-CGC-CPF               PIC  X(009).
ST2506             07  GFCTNE-FILIAL                PIC  X(004).
                   07  GFCTNE-CONTROLE              PIC  X(002).
                   07  GFCTNE-SEGMENTO              PIC  9(003).
                   07  GFCTNE-AGENCIA               PIC  9(005).
                   07  GFCTNE-POSTO                 PIC  9(005).
                   07  GFCTNE-MUNICIPIO             PIC  9(007).
                   07  GFCTNE-UF                    PIC  X(002).
                   07  GFCTNE-DESCR-AGPTO           PIC  X(030).
                   07  GFCTNE-COD-TARIFA            PIC  9(005).
                   07  GFCTNE-DESCR-TARIFA          PIC  X(015).
                   07  GFCTNE-TIMESTAMP             PIC  X(026).
                   07  GFCTNE-SEQUENCIA             PIC  9(002).
                   07  GFCTNE-SITUACAO              PIC  X(018).
                   07  GFCTNE-GRUPO-CTBIL           PIC  9(003).
                   07  GFCTNE-SUB-GRUPO-CTBIL       PIC  9(003).
                   07  GFCTNE-DESC-CTBIL            PIC  X(040).
                   07  GFCTNE-ERRO-OCORR            PIC  9(001).
                   07  GFCTNE-COD-MSG-ERRO-OCORR    PIC  9(007).
                   07  GFCTNE-COD-SQL-ERRO-OCORR    PIC  9(003).
