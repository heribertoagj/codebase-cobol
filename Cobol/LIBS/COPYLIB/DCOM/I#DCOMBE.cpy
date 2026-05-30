      ******************************************************************
      * SISTEMA : DCOM - I#DCOMBE                                      *
      *-----------------------------------------------------------------
      * COPIA DA INC I#NBDD03                                          *
      *-----------------------------------------------------------------
      *    INC  ****  I#NBDD03  - ARQUIVO DE INTERFACE NBDD            *
      *                           LAYOUT 03                            *
      *               FB - TAMANHO = 300 BYTES                         *
      *                                                                *
      *    OBS. FORMATO DAS DATAS: AAAAMMDD                            *
      *         CAMPOS ALFANUMERICOS: ALINHAR O CONTEUDO A ESQUERDA E  *
      *         COMPLETAR SE NECESSARIO COM 'BRANCOS'                  *
      *         EXEMPLO: INDEXADOR: 'PRE      '  PRE-FIXADO            *
      *                             '00034    '  NTAX                  *
      *                             'CESTA01  '  PLATAFORMA BAIXA      *
      *                                                                *
      *-----------------------------------------------------------------
      *         MANUTENCAO:                                            *
      *         DATA.... -BREVE DESCRICAO............................. *
      *                                                                *
      *-----------------------------------------------------------------
       01  HEADER-03.
      *
         05  H03-TP-REGISTRO               PIC 9(01).
      *         = 0 HEADER                                POS   1
         05  H03-TP-ARQUIVO                PIC X(03).
      *         = 'EST' ESTOQUE (FECHADO COM O CONTABIL)  POS   2
      *         = 'NOV' MOVIMENTO DE ENTRADA DE OPERACOES NOVAS
      *         = 'GER' POSICAO GERENCIAL
         05  H03-DT-HR-GERACAO             PIC X(26).
      *                                                   POS   5
      *         FORMATO TIMESTAMP: AAAA-MM-DD-HH.MM.SS.MMMMMM
         05  H03-CCUSTO                    PIC X(04).
      *         CENTRO DE CUSTO                           POS  31
         05  H03-DT-REFERENCIA             PIC 9(08).
      *         FORMATO AAAAMMDD                          POS  35
         05  H03-NM-ARQUIVO                PIC X(08).
      *         NOME DO ARQUIVO                           POS  43
         05  H03-FILLER                    PIC X(250).
      *                                                   POS  51
      *-----------------------------------------------------------------
       01  DETALHE-03 REDEFINES HEADER-03.
      *
         05  R03-TP-REGISTRO               PIC 9(01).
      *         = 1 SEM MOVIMENTO                         POS   1
      *         = 2 DETALHE
      *         = 3 DETALHE (OPERACOES EM CA CREDITO EM ATRASO)
         05  R03-CD-BANCO                  PIC 9(05).
      *         CODIGO DO BANCO, EX: 00237 - BRADESCO     POS   2
         05  R03-CD-EMPRESA                PIC 9(05).
      *         CODIGO DA EMPRESA, EX:00237 - BRADESCO    POS   7
         05  R03-CD-DEPTO                  PIC 9(05).
      *         CODIGO DO DEPARTAMENTO                    POS  12
         05  R03-CD-PRODUTO                PIC X(09).
      *         CODIGO DO PRODUTO                         POS  17
         05  R03-CD-SUB-PRODUTO            PIC X(09).
      *         CODIGO DO SUB-PRODUTO                     POS  26
         05  R03-CD-CARTEIRA               PIC X(09).
      *         CODIGO DA CARTEIRA                        POS  35
         05  R03-NM-SUB-PRODUTO            PIC X(40).
      *         NOME DO SUB-PRODUTO                       POS  44
         05  R03-TP-CLASSE                 PIC 9(01).
      *         = 1 ATIVO                                 POS  84
      *         = 2 PASSIVO
         05  R03-INTRAGRUPO                PIC X(01).
      *         'S' = SIM                                 POS  85
      *         'N' = NAO
         05  R03-MOEDA-ORIG                PIC X(09).
      *         MOEDA ORIGEM                              POS  86
      *         PREENCHER COM 'REAL     ' CASO NAO SE APLIQUE
         05  R03-INDEXADOR-ORIG            PIC X(09).
      *         INDEXADOR ORIGEM                          POS  95
         05  R03-COSIF                     PIC 9(09).
      *         COSIF                                     POS 104
         05  R03-PERC-INDEXADOR            PIC S9(09)V9(06) COMP-3.
      *         PERCENTUAL DO INDEXADOR                   POS 113
      *         PREENCHER COM ZEROS CASO NAO SE APLIQUE
         05  R03-RATING                    PIC X(03).
      *         RATING                                    POS 121
         05  R03-SEGMENTO                  PIC 9(03).
      *         SEGMENTO                                  POS 124
         05  R03-PAIS-ORIG                 PIC X(09).
      *         CODIGO DO PAIS ORIGEM                     POS 127
      *         PREENCHER COM '01058    ' CASO NAO SE APLIQUE
         05  R03-TRAVA                     PIC X(09).
      *         TRAVA                                     POS 136
      *         PREENCHER COM 'SECAS    ' CASO NAO SE APLIQUE
         05  R03-PORTFOLIO                 PIC X(01).
      *         'B' = BANKING                             POS 145
      *         'T' = TRADING
         05  R03-MERCADORIA-ORIG           PIC X(09).
      *         TIPO DE MERCADORIA ATRELADO A ORIGEM      POS 146
      *         (COMMODITIES)
         05  R03-DT-VENCTO-PC              PIC 9(08).
      *         DATA VENCIMENTO DA PARCELA                POS 155
         05  R03-VLR-PRINC-PC              PIC S9(15)V9(02) COMP-3.
      *         VALOR PRINCIPAL DA PARCELA                POS 163
         05  R03-VLR-PRINC-ORIG-PC         PIC S9(15)V9(02) COMP-3.
      *         VALOR PRINCIPAL ORIGINAL DA PARCELA       POS 172
         05  R03-VLR-PRESENTE              PIC S9(15)V9(02) COMP-3.
      *         VALOR PRESENTE                            POS 181
         05  R03-VLR-FUTURO                PIC S9(15)V9(02) COMP-3.
      *         VALOR FUTURO                              POS 190
         05  R03-VLR-PRES-CUSTO            PIC S9(15)V9(02) COMP-3.
      *         VALOR PRESENTE CUSTO                      POS 199
         05  R03-VLR-FUT-CUSTO             PIC S9(15)V9(02) COMP-3.
      *         VALOR FUTURO CUSTO                        POS 208
         05  R03-VLR-PRES-FUNDING          PIC S9(15)V9(02) COMP-3.
      *         VALOR PRESENTE FUNDING                    POS 217
         05  R03-VLR-FUT-MOEDA-ESTRANG     PIC S9(15)V9(02) COMP-3.
      *         VALOR FUTURO EM MOEDA ESTRANGEIRA         POS 226
         05  R03-FILLER                    PIC X(66).
      *                                                   POS 235
      *-----------------------------------------------------------------
      *  FIM DA INC I#DCOMBE

