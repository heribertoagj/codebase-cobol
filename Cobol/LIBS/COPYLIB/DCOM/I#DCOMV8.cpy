      *================================================================*
      *                          QINTESS                               *
      *================================================================*
      * DESCRICAO: AREA PARA REDEFINIR MSG ENVIADA/RECEBIDA AO BACEN   *
      * DATA......:  19/11/2020                                        *
      *----------------------------------------------------------------*
      *               LAYOUT PARA MENSAGEM COR0008                     *
      *----------------------------------------------------------------*
BR0521*  ANALISTA......: SIMONE - BRQ                                  *
BR0521*  DATA..........: JULHO/2027                                    *
BR0521*  OBJETIVO......: ADEQUAR PARA SICOR 5.02                       *
BR0521*  ALTERACOES NA COR0008 - INCLUSAO NUMERO REFERENCIA BACEN COR  *
      *----------------------------------------------------------------*

       01  V8-COR0008.
      **** 01 <CODMSG>                      [1..1]
           05 V8-CODMSG                 PIC  X(009) VALUE SPACES.
      **** 02 <NUMCTRLIF>                   [1..1]
           05 V8-NUMCTRLIF              PIC  X(020) VALUE SPACES.
      **** 03 <CNPJENTRESPONS>              [1..1]
           05 V8-CNPJ-ENT-RESPONS       PIC  9(014) VALUE ZEROS.
      **** 06 <DTINIPESQ>                   [1..1]
           05 V8-COR0008-DTINI-PESQ     PIC  X(007) VALUE SPACES.
      **** 07 <DTFIMPESQ>                   [1..1]
           05 V8-COR0008-DTFIM-PESQ     PIC  X(007) VALUE SPACES.
BR0521**** 08 <NUMREFBCCOR)                 [1..1]
BR0521     05 V8-COR0008-NUM-REF-BCCOR  PIC  X(011) VALUE SPACES.
      **** 10 <NUMPAGN>                     [0..1]
           05 V8-NUM-PAG                PIC  9(003) VALUE ZEROS.
           05 V8-NUM-PAG-X              REDEFINES
              V8-NUM-PAG                PIC  X(003).
      **** 11 <DTMOVTO>                     [1..1]
           05 V8-DTMOVTO                PIC  X(010) VALUE SPACES.

      *----------------------------------------------------------------*
      *    LAYOUT PARA MENSAGEM COR0008R1                              *
      *----------------------------------------------------------------*

       01  V8-COR0008R1.
           05 V8-COR0008R1-1.
              10 V8-COD-MSG         PIC X(009)            VALUE SPACES.
              10 V8-NRO-CTRL-IF     PIC X(020)            VALUE SPACES.
              10 V8-CNPJ-ENT-RESP   PIC 9(014)            VALUE ZEROS.
              10 V8-QTD-CED         PIC 9(009)            VALUE ZEROS.

           05 V8-COR0008R1-2.
              10 V8-REPET-COR0008R1-OP.
                15 V8-GRUPO-COR0008R1-OP.
                   20 FILLER                  PIC X(006) VALUE '{1000}'.
                   20 V8-NUM-REF-BCCOR        PIC X(011) VALUE SPACES.
                   20 V8-NUM-ORDEM-DESTC      PIC 9(003) VALUE ZEROS.
                   20 V8-ANO-MES-REF          PIC X(007) VALUE SPACES.
                   20 V8-SIT-OPCOR            PIC 9(002) VALUE ZEROS.
                   20 V8-MOT-RECSA-DAD-DINMCO PIC 9(003) VALUE ZEROS.
                   20 V8-DT-HR-IMPTC-SCR      PIC X(014) VALUE SPACES.

           05 V8-COR0008R1-3.
              10 V8-DTHR-BC               PIC X(014) VALUE SPACES.
              10 V8-DATA-MOVTO            PIC X(010) VALUE SPACES.

      *----------------------------------------------------------------*
      *    LAYOUT PARA MENSAGEM COR0008E                               *
      *----------------------------------------------------------------*

       01  V8-COR0008E.
           05 V8-CODMSG-R                 PIC  X(009) VALUE SPACES.
           05 V8-CODMSG-E                 PIC  X(008) VALUE SPACES.
           05 V8-NUMCTRLIF-R              PIC  X(020) VALUE SPACES.
           05 V8-NUMCTRLIF-E              PIC  X(008) VALUE SPACES.
           05 V8-CNPJ-ENT-RESPONS-R       PIC  9(014) VALUE ZEROS.
           05 V8-CNPJ-ENT-RESPONS-E       PIC  X(008) VALUE SPACES.
           05 V8-COR0008-DTINI-PESQ-R     PIC  X(007) VALUE SPACES.
           05 V8-COR0008-DTINI-PESQ-E     PIC  X(008) VALUE SPACES.
           05 V8-COR0008-DTFIM-PESQ-R     PIC  X(007) VALUE SPACES.
           05 V8-COR0008-DTFIM-PESQ-E     PIC  X(008) VALUE SPACES.
           05 V8-NUM-PAG-R                PIC  9(003) VALUE ZEROS.
           05 V8-NUM-PAG-E                PIC  X(003) VALUE SPACES.
           05 V8-DTMOVTO-R                PIC  9(008) VALUE ZEROS.
           05 V8-DTMOVTO-E                PIC  X(008) VALUE SPACES.
