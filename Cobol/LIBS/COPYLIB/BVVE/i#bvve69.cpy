      ****************************************************************
      *                                                              *
      *    NOME DA INC = I#BVVE69                                    *
      *    TAMANHO     =  300 POSICOES                               *
      *    DESCRICAO   = ARQUIVO DE VENDEDORES CADASTRADOS PELA      *
      *                  VISANET                                     *
      ****************************************************************

      *                                                     DE   ATE
      *                                                     ---  ---

       01  REG-VEND-HEADER.
      *      TIPO DE REGISTRO - 0 = HEADER                  001  001
           05  VISA-TIPO-REGH        PIC 9(01).
      *      PERIODO DO FATURAMENTO  -  AAAAMM              002  007
           05  VISA-FATURAMTO        PIC 9(06).
      *      SEQUENCIA DE ENVIO                             008  013
           05  VISA-SEQ-ENVIO        PIC 9(06).
      *      ESPACO VAGO                                    014  350
           05  FILLER                PIC X(337).


       01  REG-VEND-VISA.
      *      TIPO DE REGISTRO - 1 = DETALHE                 001  001
           05  VISA-TIPO-REGD        PIC 9(01).
      *      ESTABELECIMENTO                                002  011
           05  VISA-ESTABELEC        PIC 9(10).
      *      BANCO                                          012  015
           05  VISA-BANCO            PIC 9(04).
      *      AGENCIA                                        016  020
           05  VISA-AGENCIA          PIC 9(05).
      *      CONTA CORRENTE                                 021  034
           05  FILLER                PIC X(02).
           05  VISA-CTA-CORR         PIC 9(07).
           05  VISA-CTA-CORR-DIG     PIC X(01).
           05  FILLER                PIC X(04).
      *      NOME DO ESTABELECIMENTO                        035  066
           05  VISA-NOME-ESTABELEC   PIC X(32).
      *      C.G.C                                          067  083
           05  VISA-CGC              PIC 9(17).
      *      DATA DE ABERTURA                               084  091
           05  VISA-DATA-ABERTURA    PIC X(08).
      *      ENDERECO                                       092  123
           05  VISA-ENDERECO         PIC X(32).
      *      COMPLEMENTO                                    124  155
           05  VISA-COMPLEMENTO      PIC X(32).
      *      CIDADE                                         156  183
           05  VISA-CIDADE           PIC X(28).
      *      UF                                             184  185
           05  VISA-UF               PIC X(02).
      *      CEP                                            186  194
           05  VISA-CEP              PIC 9(09).
      *      RAMO DE ATIVIDADE                              195  199
           05  VISA-RAMO-ATIV        PIC 9(05).
      *      STATUS - VIDE DEFIN. ABAIXO                    200  200
           05  VISA-STATUS           PIC X(01).
      *      CATEGORIA DE ATIVIDADE                         201  201
           05  VISA-CATEG-ANTEC      PIC X(01).
      *      FLAG DE POS - 'S' OU 'N'                       202  202
           05  VISA-FLAG-POS         PIC X(01).
      *      NOME DO CONTATO                                203  234
           05  VISA-NOME-CONTATO     PIC X(32).
      *      TELEFONE DE CONTATO                            235  252
           05  VISA-DDD-CONTATO      PIC 9(07).
           05  VISA-NUMERO-TELEF     PIC 9(11).
      *      TAXA - 3 CASAS DECIMAIS                        253  259
           05  VISA-TAXA             PIC 9(03)V9999.
      *      PRAZO                                          260  261
           05  VISA-PRAZO            PIC 9(02).
      *      DATA DE PROCESSAMENTO                          262  269
           05  VISA-DATA-PROC        PIC 9(08).
      *      SINAL VLR BRUTO - (-) OU (+)                   270  270
           05  VISA-SINAL-VR-BRUTO   PIC X(01).
      *      VALOR BRUTO                                    271  285
           05  VISA-VALOR-BRUTO      PIC 9(13)V99.
      *      SINAL VLR LIQUIDO - (-) OU (+)                 286  286
           05  VISA-SINAL-FATURA     PIC X(01).
      *      VALOR LIQUIDO                                  287  301
           05  VISA-FATURA-MES       PIC 9(13)V99.
      *      ESPACO VAGO                                    302  350
           05  FILLER                PIC X(49).

       01  REG-VEND-TRAILLER.
      *      TIPO DE REGISTRO - 9 = TRAILLER                001  001
           05  VISA-TIPO-REGT        PIC 9(01).
      *      QTDE TOTAL DE ESTABELECIMENTOS                 002  010
           05  VISA-QTDE-ESTAB       PIC 9(09).
      *      SINAL DO VLR TOTAL BRUTO                       011  011
           05  VISA-SINAL-TOT-BRUTO  PIC X(01).
      *      VALOR TOTAL BRUTO                              012  026
           05  VISA-VLR-TOT-BRUTO    PIC 9(13)V99.
      *      SINAL DO VLR TOTAL LIQUIDO                     027  027
           05  VISA-SINAL-TOT-LIQUI  PIC X(01).
      *      VALOR TOTAL LIQUIDO                            028  042
           05  VISA-VLR-TOT-LIQUI    PIC 9(13)V99.
      *      ESPACO VAGO                                    043  350
           05  FILLER                PIC X(308).

      **********************************************
      *  STATUS DE ATIVIDADE DO ESTABELECIMENTO :  *
      * ===========================================*
      *  O = OPEN                                  *
      *  F = FROZEN                                *
      *  C = CLOSE                                 *
      *  R = REOPENED                              *
      *  T = TRANFER                               *
      *  D = DESACTIVATED                          *
      *  P = NEW ACCOUNT PENDING                   *
      **********************************************
