      ******************************************************************
      * COPIA DA I#GBELEZ                                              *
      * I#DCOMWI  --> EXTRACAO DE PAGAMENTOS - BASILEIA II             *
      * LRECL=170 --> 06/2010  -  GPTI                                 *
      * DESCRICAO --> LAY-OUT  PARA REMESSA DOS PAGAMENTOS EFETUADOS,  *
      *              PELOS CONTRATOS INADIMPLENTES E EM DEFALULT       *
      *              (RENEGOCIACAO, CONCORDATA, FALENCIA E RECUPERACAO *
      *               JUDICIAL) PARA MAINFRAME.                        *
      *                                                                *
      * TABELA    - CODIGOS PGTOS DA PARCELA E CONTRATO:               *
      *        01 - PAGAMENTO PARCIAL                                  *
      *        02 - PAGAMENTO TOTAL OU QUITACAO                        *
      *        03 - BAIXA POR RENEGOCIACAO                             *
      *        04 - BAIXA POR SECURITIZACAO                            *
      *        05 - BAIXA POR FRAUDE                                   *
      *        06 - BAIXA POR CANCELAMENTO                             *
      *        07 - ESTORNO DE BAIXA                                   *
      *        08 - BAIXA POR AVALIACAO DO BEM (BNDU)                  *
      *        09 - BAIXA POR VENDA DO BEM (BNDU)                      *
      *        10 - BAIXA POR ACORDO                                   *
      *        11 - AMORTIZACAO DE SALDO DEVEDOR (APENAS PARA EMPF)    *
      *        12 - BAIXA POR AVALIACAO DO BEM - PARCIAL               *
      *        13 - BAIXA POR EXPURGO (APENAS PARA CLLP)               *
      *                                                                *
      ******************************************************************
      *
       01  DCOMWI-REG-HEADER.
           05  DCOMWI-HEA-TP-LAYOUT       PIC X(01).
           05  DCOMWI-HEA-NOME-EMPRESA    PIC X(10).
           05  DCOMWI-HEA-DT-PROCESSA     PIC X(10).
           05  DCOMWI-HEA-CCUSTO          PIC X(04).
           05  DCOMWI-HEA-SEQ-REG         PIC 9(12).
           05  DCOMWI-HEA-COD-REMESSA     PIC 9(02).
           05  DCOMWI-HEA-CNPJ-CGC        PIC 9(15).
           05  DCOMWI-HEA-COD-ERROS       PIC X(14).
           05  FILLER                     PIC X(102).
      *
       01  DCOMWI-REG-DETALHE REDEFINES DCOMWI-REG-HEADER.
           05  DCOMWI-DET-TP-LAYOUT       PIC X(01).
           05  DCOMWI-DET-COD-EMPRESA     PIC 9(05).
           05  DCOMWI-DET-AGENCIA         PIC 9(05).
           05  DCOMWI-DET-CONTA           PIC 9(13).
           05  DCOMWI-DET-CARTEIRA        PIC X(03).
           05  DCOMWI-DET-CONTRATO        PIC 9(17).
           05  DCOMWI-DET-DT-VENCIMENTO   PIC X(10).
           05  DCOMWI-DET-DT-PAGAMENTO    PIC X(10).
           05  DCOMWI-DET-VL-PAGAMENTO    PIC 9(15)V99.
           05  DCOMWI-DET-COD-PG-CONTRATO PIC 9(02).
           05  DCOMWI-DET-COD-PG-PARCELA  PIC 9(02).
           05  DCOMWI-DET-DT-BASE         PIC 9(06).
           05  DCOMWI-DET-PRODUTO         PIC 9(03).
           05  DCOMWI-DET-NUM-PARCELA     PIC 9(03).
           05  DCOMWI-DET-CNPJ-CPF        PIC 9(15).
           05  DCOMWI-DET-SEQ-REGISTRO    PIC 9(12).
           05  DCOMWI-DET-COD-ERROS       PIC X(34).
           05  FILLER                     PIC X(12).
      *
       01  DCOMWI-REG-TRAILLER REDEFINES DCOMWI-REG-HEADER.
           05  DCOMWI-TRA-TP-LAYOUT       PIC X(01).
           05  DCOMWI-TRA-QTDE-DET        PIC 9(08).
           05  DCOMWI-TRA-QTDE-REG        PIC 9(08).
           05  DCOMWI-TRA-SEQ-REG         PIC 9(12).
           05  DCOMWI-TRA-COD-ERROS       PIC X(08).
           05  FILLER                     PIC X(133).
      *
      ******************************************************************
      *SEQ  LVL  FIELD NAME                FORMAT       START  LENGTH  *
      ******************************************************************
      *  1  01 DCOMWI-REG-HEADER          G                 1     170  *
      *  2   05 DCOMWI-HEA-TP-LAYOUT      C     1           1       1  *
      *  3   05 DCOMWI-HEA-NOME-EMPRESA   C    10           2      10  *
      *  4   05 DCOMWI-HEA-DT-PROCESSA    C    10          12      10  *
      *  5   05 DCOMWI-HEA-CCUSTO         C     4          22       4  *
      *  6   05 DCOMWI-HEA-SEQ-REG        Z    12          26      12  *
      *  7   05 DCOMWI-HEA-COD-REMESSA    Z     2          38       2  *
      *  8   05 DCOMWI-HEA-CNPJ-CGC       Z    15          40      15  *
      *  9   05 DCOMWI-HEA-COD-ERROS      C    14          55      14  *
      * 10   05 FILLER                    C   102          69     102  *
      *                                                                *
      * 12   05 DCOMWI-DET-TP-LAYOUT      C     1           1       1  *
      * 13   05 DCOMWI-DET-COD-EMPRESA    Z     5           2       5  *
      * 14   05 DCOMWI-DET-AGENCIA        Z     5           7       5  *
      * 15   05 DCOMWI-DET-CONTA          Z    13          12      13  *
      * 16   05 DCOMWI-DET-CARTEIRA       C     3          25       3  *
      * 17   05 DCOMWI-DET-CONTRATO       Z    17          28      17  *
      * 18   05 DCOMWI-DET-DT-VENC        C    10          45      10  *
      * 19   05 DCOMWI-DET-DT-PAG         C    10          55      10  *
      * 20   05 DCOMWI-DET-VL-PAG         Z    15  2       65      17  *
      * 21   05 DCOMWI-DET-COD-PG-CONT    Z     2          82       2  *
      * 22   05 DCOMWI-DET-COD-PG-PARC    Z     2          84       2  *
      * 23   05 DCOMWI-DET-DT-BASE        Z     6          86       6  *
      * 24   05 DCOMWI-DET-PRODUTO        Z     3          92       3  *
      * 25   05 DCOMWI-DET-NUM-PARCELA    Z     3          95       3  *
      * 26   05 DCOMWI-DET-CNPJ-CPF       Z    15          98      15  *
      * 27   05 DCOMWI-DET-SEQ-REGISTRO   Z    12         113      12  *
      * 28   05 DCOMWI-DET-COD-ERROS      C    34         125      34  *
      * 29   05 FILLER                    C    12         159      12  *
      *                                                                *
      * 31   05 DCOMWI-TRA-TP-LAYOUT      C     1           1       1  *
      * 32   05 DCOMWI-TRA-QTDE-DET       Z     8           2       8  *
      * 33   05 DCOMWI-TRA-QTDE-REG       Z     8          10       8  *
      * 34   05 DCOMWI-TRA-SEQ-REG        Z    12          18      12  *
      * 35   05 DCOMWI-TRA-COD-ERROS      C     8          30       8  *
      * 36   05 FILLER                    C   133          38     133  *
      ******************************************************************
