      ******************************************************************
      *    NOME BOOK : CRURW07V                                        *
      *    DESCRICAO : BOOK DE INTERFACE                               *
      *    DATA      : 18/03/2013                                      *
      *    AUTOR     : DAVI RODRIGUES                                  *
      *    EMPRESA   : CAPGEMINI                                       *
      *    GRUPO     : CAPGEMINI                                       *
      *    TAMANHO   : 1024 BYTES                                      *
      *                                                                *
      ******************************************************************
      *================================================================*
B0923 * ANALISTA REPONSAVEL: BELGA JUNIOR - FOURSYS                    *
B0923 * DATA...............: SETEMBRO - 2023                           *
B0923 * OBJETIVOS..........: RESOLUCAO 5.078 - REDUTOR DE TAXA - CAR   *
      *================================================================*
STE002* ANALISTA REPONSAVEL: VICTORINO - STEFANINI                     *
STE002* DATA...............: SETEMBRO - 2024                           *
STE002* OBJETIVOS..........: RESOLUCAO 5.08 - INDICADORES DE CONFORM.  *
      *================================================================*
STE003* ANALISTA REPONSAVEL: VICTORINO - STEFANINI                     *
STE003* DATA...............: OUTUBRO - 2024                            *
STE003* OBJETIVOS..........: RESOLUCAO SICOR 5.09                      *
      *================================================================*
      *                                                                *
      ******************************************************************
      *
           05 CRURW07V-REGISTROS.
             10 CRURW07V-CEMPTO-RURAL         PIC  9(006).
             10 CRURW07V-CIMOV-CREDT-RURAL    PIC  9(010).
             10 CRURW07V-MAREA-FINCD-RURAL    PIC  9(007)V9(04).
             10 CRURW07V-CUND-AREA-FINCD      PIC  9(003).
             10 CRURW07V-QITEM-FINCD-RURAL    PIC  9(007)V9(04).
             10 CRURW07V-CUND-ITEM-FINCD      PIC  9(003).
             10 CRURW07V-QPROTV-IMOV-BENEF    PIC  9(007)V9(04).
             10 CRURW07V-CUND-PROTV-IMOV      PIC  9(003).
             10 CRURW07V-IND-ZONA-RURAL-BACEN PIC  9(001).
             10 CRURW07V-COD-IMOVEL-REC-FDRAL PIC  X(009).
             10 CRURW07V-COD-IMOVEL-CAD-RURAL PIC  X(013).
             10 CRURW07V-NUM-CAD-AMBIE-RURAL  PIC  X(041).
             10 CRURW07V-IND-CAD-AMBIE-RURAL  PIC  X(001).
STE002       10 CRURW07V-IND-CONF-IBAMA       PIC  X(001).
STE002       10 CRURW07V-IND-CONF-UC          PIC  X(001).
STE002       10 CRURW07V-IND-CONF-FLOR-PUB    PIC  X(001).
STE003       10 CRURW07V-IND-CONF-IMOV        PIC  X(001).
STE003       10 CRURW07V-TPO-CONF             PIC  X(005).
STE002       10 CRURW07V-DESCRITIVO           PIC  X(300).
VI0725       10 CRURW07V-QTDTOTAL             PIC  X(010).
VI0725       10 CRURW07V-QTDTOTAL-UND-ANML    PIC  X(007).
VI0725       10 CRURW07V-OCORRENCIAS          PIC  X(003).
VI0725       10 CRURW07V-QTDANIMAL            PIC  X(050).
VI0725       10 CRURW07V-CODCATANIMAL         PIC  X(080).
VI0725       10 CRURW07V-UNDDANIMAL           PIC  X(070).
VI0725       10 CRURW07V-TOTUNDDANIMAL        PIC  X(060).
VI0725       10 CRURW07V-TIPO-CONFNOVO        OCCURS 20 TIMES.
VI0725         15 CRURW07V-TIPO-CONFNOVO-N    PIC  X(003).
VI0725       10 FILLER                        PIC  X(252).
