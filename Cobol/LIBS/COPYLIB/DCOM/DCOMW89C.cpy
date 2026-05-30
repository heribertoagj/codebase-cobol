      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - DCOMW89C
      * Data/Hora : 13/Dez/2017 14:25:15
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
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
