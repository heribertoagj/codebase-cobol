      ***===========================================================***
      *** NOME INC                                                  ***
      *** GFCTWAD0                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: AREA PADRAO PARA DISPLAY DE ESTATISTICAS DE    ***
      ***            PROCESSAMENTO DE PROGRAMAS                     ***
      ***                                                           ***
      ***===========================================================***
       01 WRK-DISPLAY-PADRAO.
         03 WRK-DISP1.
           05 FILLER                   PIC X(70) VALUE ALL '*'.

         03 WRK-DISP2.
           05 FILLER                   PIC X(02) VALUE ALL '*'.
           05 FILLER                   PIC X(19) VALUE ALL ' '.
           05 FILLER                   PIC X(29) VALUE
            'ESTATISTICAS DE PROCESSAMENTO'.
           05 FILLER                   PIC X(18) VALUE ALL ' '.
           05 FILLER                   PIC X(02) VALUE ALL '*'.

         03 WRK-DISP3.
           05 FILLER                   PIC X(02) VALUE ALL '*'.
           05 FILLER                   PIC X(66) VALUE ALL '-'.
           05 FILLER                   PIC X(02) VALUE ALL '*'.

         03 WRK-DISP4.
           05 FILLER                   PIC X(03) VALUE '** '.
           05 FILLER                   PIC X(08) VALUE 'PROGRAMA'.
           05 FILLER                   PIC X(03) VALUE ' | '.
           05 FILLER                   PIC X(09) VALUE 'JOBNAME  '.
           05 FILLER                   PIC X(03) VALUE ' | '.
           05 FILLER                   PIC X(21) VALUE
            'DATA DE PROCESSAMENTO'.
           05 FILLER                   PIC X(03) VALUE ' | '.
           05 FILLER                   PIC X(18) VALUE SPACES.
           05 FILLER                   PIC X(02) VALUE '**'.

         03 WRK-DISP5.
           05 FILLER                   PIC X(03) VALUE '** '.
           05 FILLER                   PIC X(08) VALUE '--------'.
           05 FILLER                   PIC X(03) VALUE ' | '.
           05 FILLER                   PIC X(09) VALUE '-------- '.
           05 FILLER                   PIC X(03) VALUE ' | '.
           05 FILLER                   PIC X(21) VALUE
            '---------------------'.
           05 FILLER                   PIC X(03) VALUE ' | '.
           05 FILLER                   PIC X(18) VALUE
            '----------------- '.
           05 FILLER                   PIC X(02) VALUE '**'.

         03 WRK-DISP6.
           05 FILLER                   PIC X(03) VALUE '** '.
           05 WRK-PROGNAME             PIC X(08).
           05 FILLER                   PIC X(03) VALUE ' | '.
           05 WRK-JOBNAME              PIC X(08).
           05 FILLER                   PIC X(05) VALUE '  |  '.
           05 WRK-DATAMOV              PIC X(10).
           05 FILLER                   PIC X(01) VALUE SPACES.
           05 WRK-HORAMOV              PIC X(08) VALUE SPACES.
           05 FILLER                   PIC X(08) VALUE '  |     '.
           05 FILLER                   PIC X(10) VALUE SPACES.
           05 FILLER                   PIC X(06) VALUE '    **'.

         03 WRK-DISP7.
           05 FILLER                   PIC X(04) VALUE '**  '.
           05 FILLER                   PIC X(07) VALUE 'DDNAME '.
           05 FILLER                   PIC X(03) VALUE ' | '.
           05 FILLER                   PIC X(03) VALUE 'I/O'.
           05 FILLER                   PIC X(03) VALUE ' | '.
           05 FILLER                   PIC X(27) VALUE
            'DESCRICAO DO ARQUIVO       '.
           05 FILLER                   PIC X(03) VALUE ' | '.
           05 FILLER                   PIC X(18) VALUE
            '    QUANTIDADE    '.
           05 FILLER                   PIC X(02) VALUE '**'.

         03 WRK-DISP8.
           05 FILLER                   PIC X(03) VALUE '** '.
           05 FILLER                   PIC X(08) VALUE '--------'.
           05 FILLER                   PIC X(03) VALUE ' | '.
           05 FILLER                   PIC X(03) VALUE '---'.
           05 FILLER                   PIC X(03) VALUE ' | '.
           05 FILLER                   PIC X(27) VALUE
            '---------------------------'.
           05 FILLER                   PIC X(03) VALUE ' | '.
           05 FILLER                   PIC X(18) VALUE
            '----------------- '.
           05 FILLER                   PIC X(02) VALUE '**'.

         03 WRK-DISP9.
           05 FILLER                   PIC X(03) VALUE '** '.
           05 WRK-DDNAME               PIC X(08).
           05 FILLER                   PIC X(04) VALUE ' |  '.
           05 WRK-I-O                  PIC X(01).
           05 FILLER                   PIC X(04) VALUE '  | '.
           05 WRK-DESCARQ              PIC X(27).
           05 FILLER                   PIC X(05) VALUE ' |   '.
           05 WRK-QTDEARQ              PIC ZZZ.ZZZ.ZZZ.ZZ9.
           05 FILLER                   PIC X(03) VALUE ' **'.

         03 WRK-DISP10.
           05 FILLER                   PIC X(04) VALUE '**  '.
           05 FILLER                   PIC X(07) VALUE 'TABELA '.
           05 FILLER                   PIC X(03) VALUE ' | '.
           05 FILLER                   PIC X(03) VALUE 'FUN'.
           05 FILLER                   PIC X(03) VALUE ' | '.
           05 FILLER                   PIC X(27) VALUE
            'DESCRICAO DA TABELA        '.
           05 FILLER                   PIC X(03) VALUE ' | '.
           05 FILLER                   PIC X(18) VALUE
            '    QUANTIDADE    '.
           05 FILLER                   PIC X(02) VALUE '**'.

         03 WRK-DISP11.
           05 FILLER                   PIC X(03) VALUE '** '.
           05 WRK-TABLENAME            PIC X(08).
           05 FILLER                   PIC X(04) VALUE ' |  '.
           05 WRK-FUN                  PIC X(01).
           05 FILLER                   PIC X(04) VALUE '  | '.
           05 WRK-DESCTAB              PIC X(27).
           05 FILLER                   PIC X(05) VALUE ' |   '.
           05 WRK-QTDETAB              PIC ZZZ.ZZZ.ZZZ.ZZ9.
           05 FILLER                   PIC X(03) VALUE ' **'.
