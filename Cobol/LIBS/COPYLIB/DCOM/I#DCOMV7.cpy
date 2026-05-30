      *================================================================*
      * AREA PARA REDEFINIR MENSAGEM ENVIADA / RECEBIDA AO BACEN       *
      * LAYOUT AUXILIAR PARA O CAMPO M0-WCONTD-MSGEM-DESC-TEXT.        *
      *----------------------------------------------------------------*
      * OBSERVACOES:                                                   *
      *   - CAMPOS DE DATA NO FORMATO AAAAMMDD.                        *
      *   - CAMPOS DE VALOR POSITIVOS SEM SINAL.                       *
      *================================================================
      *----------------------------------------------------------------*
      *    LAYOUT PARA MENSAGEM COR0007                                *
      *----------------------------------------------------------------*
QT1120*                    ALTERACAO - QINTESS                         *
QT1120*----------------------------------------------------------------*
QT1120*  ANALISTA QINTESS: MARILIA SANTOS                              *
QT1120*  ANALISTA DS....:  PAULO ROBERTO SARTORATO                     *
QT1120*  DATA...........:  11/11/2020                                  *
QT1120*----------------------------------------------------------------*
QT1120*  OBJETIVO......:   ADEQUAR PARA SICOR 5.01                     *
QT1120*    ALTERACOES NA COR0007:                                      *
QT1120*                    - INCLUSAO DO CAMPO "INDICADOR PESQUISA     *
QT1120*                                           CEDULAS EMITIDAS IF" *
QT1120*================================================================*
BR0521*  OBJETIVO......:   ADEQUAR PARA SICOR 5.02                     *
BR0521*    ALTERACOES NA COR0007:                                      *
BR0521*       - INCL "INDICADOR AUTORIZACAO BENEFICIARIO"              *
BR0521*    ALTERACOES NA COR0007R1:                                    *
BR0521*       - INCL "CNPJ ENTIDADE RESPONSAVEL ORIGINAL"              *
BR0521*       - INCL "CNPJ PARTICIPANTE"                               *
BR0521*================================================================*

       01  V7-COR0007.
      **** 01 <CODMSG>                      [1..1]
           05 V7-CODMSG                 PIC  X(009) VALUE SPACES.
      **** 02 <NUMCTRLIF>                   [1..1]
           05 V7-NUMCTRLIF              PIC  X(020) VALUE SPACES.
      **** 03 <CNPJENTRESPONS>              [1..1]
           05 V7-CNPJ-ENT-RESPONS       PIC  9(014) VALUE ZEROS.
      **** 04 <TPPESSOABENFCRIO>            [0..1]
           05 V7-COR0007-TPBENEFC       PIC  X(001) VALUE ZEROS.
      **** 05 <CNPJ_CPFBENFCRIO>            [0..1]
           05 V7-COR0007-CPFBENEF       PIC  9(014) VALUE ZEROS.
           05 V7-COR0007-CPFBENEF-X     REDEFINES
              V7-COR0007-CPFBENEF       PIC  X(014).
      **** 06 <DTINIPESQ>                   [1..1]
           05 V7-COR0007-DTINI-PESQ     PIC  9(008) VALUE ZEROS.
      **** 07 <DTFIMPESQ>                   [1..1]
           05 V7-COR0007-DTFIM-PESQ     PIC  9(008) VALUE ZEROS.
      **** 08 <NUMREFBCCOR>                 [0..1]
           05 V7-NUM-REF-BACEN          PIC  X(011) VALUE SPACES.
QT1120**** 09 <INDRPESQCEDLEMITDIF>
           05 V7-IND-CEDULA-EMI-IF      PIC  X(001) VALUE SPACES.
      **** 10 <NUMPAGN>                     [0..1]
           05 V7-NUM-PAG                PIC  9(003) VALUE ZEROS.
           05 V7-NUM-PAG-X              REDEFINES
              V7-NUM-PAG                PIC  X(003).
BR0521**** 11 <INDRAUTCBENFCRIO>            [0..1]
           05 V7-IND-AUTORZ-BENEF       PIC  X(001) VALUE SPACES.
      **** 12 <DTMOVTO>                     [1..1]
           05 V7-DTMOVTO                PIC  9(008) VALUE ZEROS.

      *----------------------------------------------------------------*
      *    LAYOUT PARA MENSAGEM COR0007R1                              *
      *----------------------------------------------------------------*

       01  V7-COR0007R1.
1          05 V7-COR0007R1-1.
              10 V7-COD-MSG         PIC X(009)            VALUE SPACES.
2             10 V7-NRO-CTRL-IF     PIC X(020)            VALUE SPACES.
3             10 V7-CNPJ-ENT-RESP   PIC 9(014)            VALUE ZEROS.
4             10 V7-QTD-CED         PIC 9(009)            VALUE ZEROS.

           05 V7-COR0007R1-2.
              10 V7-REPET-COR0007R1-OP.
                15 V7-GRUPO-COR0007R1-OP.
                   20 FILLER                  PIC X(006) VALUE '{1000}'.
BR0521             20 V7-CNPJ-ENT-RESP-OR     PIC 9(014) VALUE ZEROS.
BR0521             20 V7-CNPJ-PART            PIC 9(014) VALUE ZEROS.
                   20 V7-NUM-REF-BCCOR        PIC X(011) VALUE SPACES.
                   20 V7-NUM-ORDEM-DESTC      PIC X(003) VALUE SPACES.
                   20 V7-VLR-PARCL-CRED       PIC X(019) VALUE SPACES.
                   20 V7-ANO-MES-REF          PIC X(007) VALUE SPACES.
                   20 V7-IDENTF-SCR           PIC X(044) VALUE SPACES.
                   20 V7-VLR-SLDMEDDIARIO     PIC X(019) VALUE SPACES.
                   20 V7-VLR-SLDULTDIA        PIC X(019) VALUE SPACES.
                   20 V7-VLR-SLDPRICULTDIA    PIC X(019) VALUE SPACES.
                   20 V7-VLR-SLDVINCNDULTDIA  PIC X(019) VALUE SPACES.
                   20 V7-VLR-SLDPRIVINULTDIA  PIC X(019) VALUE SPACES.
                   20 V7-VLR-SLDMEDDIAVIN     PIC X(019) VALUE SPACES.
                   20 V7-SIT-OPCOR            PIC X(002) VALUE SPACES.
                   20 V7-TP-BONUSREBT         PIC X(002) VALUE SPACES.
                   20 V7-DT-PGTO-BONUS        PIC X(008) VALUE SPACES.
                   20 V7-VLR-BONUSREBT        PIC X(019) VALUE SPACES.

           05 V7-COR0007R1-3.
              10 V7-DTHR-BC               PIC X(014) VALUE SPACES.
              10 V7-DATA-MOVTO            PIC X(008) VALUE SPACES.

      *----------------------------------------------------------------*
      *    LAYOUT PARA MENSAGEM COR0007E                               *
      *----------------------------------------------------------------*

       01  V7-COR0007E.
           05 V7-CODMSG-R                 PIC  X(009) VALUE SPACES.
           05 V7-CODMSG-E                 PIC  X(008) VALUE SPACES.
           05 V7-NUMCTRLIF-R              PIC  X(020) VALUE SPACES.
           05 V7-NUMCTRLIF-E              PIC  X(008) VALUE SPACES.
           05 V7-CNPJ-ENT-RESPONS-R       PIC  9(014) VALUE ZEROS.
           05 V7-CNPJ-ENT-RESPONS-E       PIC  X(008) VALUE SPACES.
           05 V7-COR0007-TPBENEFC-R       PIC  X(001) VALUE ZEROS.
           05 V7-COR0007-TPBENEFC-E       PIC  X(008) VALUE SPACES.
           05 V7-COR0007-CPFBENEF-R       PIC  9(014) VALUE ZEROS.
           05 V7-COR0007-CPFBENEF-E       PIC  X(008) VALUE SPACES.
           05 V7-COR0007-DTINI-PESQ-R     PIC  9(008) VALUE ZEROS.
           05 V7-COR0007-DTINI-PESQ-E     PIC  X(008) VALUE SPACES.
           05 V7-COR0007-DTFIM-PESQ-R     PIC  9(008) VALUE ZEROS.
           05 V7-COR0007-DTFIM-PESQ-E     PIC  X(008) VALUE SPACES.
           05 V7-NUM-REF-BACEN-R          PIC  X(011) VALUE SPACES.
           05 V7-NUM-REF-BACEN-E          PIC  X(008) VALUE SPACES.
QT1120     05 V7-IND-CEDULA-EMI-IF-R      PIC  X(001) VALUE SPACES.
QT1120     05 V7-IND-CEDULA-EMI-IF-E      PIC  X(008) VALUE SPACES.
           05 V7-NUM-PAG-R                PIC  9(003) VALUE ZEROS.
           05 V7-NUM-PAG-E                PIC  X(003) VALUE SPACES.
           05 V7-DTMOVTO-R                PIC  9(008) VALUE ZEROS.
           05 V7-DTMOVTO-E                PIC  X(008) VALUE SPACES.
