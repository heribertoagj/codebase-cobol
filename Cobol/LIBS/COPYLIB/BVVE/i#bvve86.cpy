      *---------------------------------------------------------------*
      * BOOK DO ARQUIVO ATENDIMENTO A CLIENTES                        *
      * TAMANHO : 0250                                                *
      * I#BACC86                          ULTIMA ALTERACAO : 18/04/95 *
      *---------------------------------------------------------------*
      *                                                         POS TAM
       01  B086-REGISTRO.
      *                                                         001 250
           05  B086-DATARECE.
      *                                                         001 008
               10  B086-SECRECE        PIC  9(002).
      *                                                         001 002
               10  B086-ANORECE        PIC  9(002).
      *                                                         003 002
               10  B086-MESRECE        PIC  9(002).
      *                                                         005 002
               10  B086-DIARECE        PIC  9(002).
      *                                                         007 002
           05  B086-NUMEVEND           PIC  9(013).
           05  B086-BANCO              PIC  9(005).
      *                                                         022 005
           05  B086-AGENCIA            PIC  9(005).
      *                                                         027 005
           05  B086-RAZAO              PIC  9(005).
      *                                                         032 005
           05  B086-CONTA              PIC  9(007).
      *                                                         037 007
           05  B086-NUMREMES           PIC  9(007).
      *                                                         044 007
           05  B086-NUMEDOC            PIC  9(007).
      *                                                         051 007
           05  B086-DATADOC.
      *                                                         058 008
               10  B086-SECDOC         PIC  9(002).
      *                                                         058 002
               10  B086-ANODOC         PIC  9(002).
      *                                                         060 002
               10  B086-MESDOC         PIC  9(002).
      *                                                         062 002
      *--- B086-VALOR = VALOR PRINCIPAL + VALOR SERVICOS ---*   064 002
           05  B086-VALOR              PIC  9(009)V99.
      *                                                         066 011
           05  B086-MOTREJEI           PIC  X(077).
      *                                                         077 077
           05  B086-PORT-ORIG          PIC  X(019).
      *                                                         154 019
           05  B086-CODREJEI           PIC  9(003).
      *                                                         173 003
           05  B086-CODLAYOUT          PIC  X(003).
      *                                                         176 003
           05  B086-LAYOUTVAR          PIC  X(047).
      *                                                         179 047
           05  B086-SUBC-ORIG          PIC  9(004).
      *                                                         226 004
           05  B086-VLR-SERV           PIC  9(009)V99 COMP-3.
      *                                                         230 006
           05  B086-FILLER             PIC  X(015).
      *                                                         236 015
