      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMW85C
      * Data/Hora : 21/Dez/2017 14:35:44
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
       05 DCOMW85A-OCORRENCIA
           OCCURS 1 TO 50 TIMES
           DEPENDING ON DCOMW85A-NOCOR.
          10 DCOMW85A-CD-LANC                                   PIC X(8)
              VALUE 'DC04L'.
          10 DCOMW85A-CTITLO-COBR-BCO                         PIC X(20).
          10 DCOMW85A-NOME-SACADO                             PIC X(40).
          10 DCOMW85A-CNPJ-SAC                                 PIC 9(9).
          10 DCOMW85A-FILIAL-SAC                               PIC 9(4).
          10 DCOMW85A-CTRL-SAC                                 PIC 9(2).
          10 DCOMW85A-DVCTO-TITLO                             PIC X(10).
          10 DCOMW85A-VALOR-IOF                               PIC 9(15).
          10 DCOMW85A-VALOR-JUROS                             PIC 9(15).
          10 DCOMW85A-VTITLO-DESC                             PIC 9(15).
          10 DCOMW85A-VALOR-LIQUIDO                           PIC 9(15).
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
