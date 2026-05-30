      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMW89A
      * Data/Hora : 13/Dez/2017 14:45:48
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
       05 DCOMW89A-CD-FRMUL                                     PIC X(8)
           VALUE 'DCOMW89A'.
       05 DCOMW89A-CBAR-01                                     PIC 9(9).
       05 DCOMW89A-CBAR-02                                     PIC 9(1).
       05 DCOMW89A-CBAR-03                                     PIC 9(6).
       05 DCOMW89A-AGENCIA                                     PIC 9(5).
       05 DCOMW89A-DIGITO-AG                                   PIC X(2).
       05 DCOMW89A-NOME-AG                                    PIC X(20).
       05 DCOMW89A-CONTA-CORR                                 PIC 9(13).
       05 DCOMW89A-DIGITO-CONTA                                PIC X(2).
       05 DCOMW89A-CNPJ                                        PIC 9(9).
       05 DCOMW89A-FILIAL                                      PIC 9(4).
       05 DCOMW89A-CTRL                                        PIC 9(2).
       05 DCOMW89A-NR-CONTR                                   PIC 9(13).
       05 DCOMW89A-DATA-INI                                   PIC X(10).
       05 DCOMW89A-VALOR-LIM                                  PIC 9(17).
       05 DCOMW89A-VL-CONTR-EXT                              PIC X(200).
       05 DCOMW89A-NOME-CLIENTE                               PIC X(40).
       05 DCOMW89A-ENDERECO                                   PIC X(40).
       05 DCOMW89A-CIDADE                                     PIC X(20).
       05 DCOMW89A-ESTADO                                      PIC X(2).
       05 DCOMW89A-TIPO-VIA                                    PIC 9(1).
       05 DCOMW89A-NOCOR                                       PIC 9(2).
       05 DCOMW89A-OCORRENCIA
           OCCURS 1 TO 4 TIMES
           DEPENDING ON DCOMW89A-NOCOR.
          10 DCOMW89A-CSF                                       PIC X(8)
              VALUE 'DC08L'.
          10 DCOMW89A-NOME-FIADOR                             PIC X(40).
          10 DCOMW89A-CPFPJ-FIADOR                            PIC X(19).
          10 DCOMW89A-TP-DOC                                   PIC X(2).
          10 DCOMW89A-RG-FIADOR                               PIC X(15).
          10 DCOMW89A-EMISS-RG                                 PIC X(5).
          10 DCOMW89A-UF-RG                                    PIC X(2).
          10 DCOMW89A-ENDERECO-FIADOR                         PIC X(40).
          10 DCOMW89A-CIDADE-FIADOR                           PIC X(20).
          10 DCOMW89A-ESTADO-FIADOR                            PIC X(2).
          10 DCOMW89A-NOME-CONJUGE                            PIC X(40).
          10 DCOMW89A-CPFPJ-CNJG                              PIC X(19).
          10 DCOMW89A-NOME-TESTEMUNHA                         PIC X(40).
          10 DCOMW89A-CPFPJ-TEST                              PIC X(19).
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
