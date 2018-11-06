#include "multiput2.bi"
#include "../inc/fbsound.bi"
#include "fbgfx.bi"
#include "windows.bi"
#include once "win/mmsystem.bi"
#if __FB_LANG__ = "fb"
    using FB
#endif

freeConsole()

type particle
    active as integer
    x as single
    y as single
    w as integer
    h as integer
    velX as single
    velY as single
    life as integer
    spd as single
    frm as integer
    chg as integer
    uvy as integer
    destx as integer
end type

type structure
    x as single
    y as single
    w as single
    h as single
    v as integer
    sects as integer
    life as integer
    m as integer
    velX as single
    vely as single
    tick as integer
end type

type agent
    active as integer
    die as integer
    diefrm as integer
    res as integer
    frm as integer
    x as single
    y as single
    w as single
    h as single
    velX as single
    velY as single
    jmp as integer
    lnch as integer
    op as integer
    chg as integer
end type

type icon
    active as integer
    x as single
    y as single
    coll as integer
    f as integer
    velX as single
    velY as single
    xwidth as integer
end type
    
'some useful defines
Const Pi = 4 * Atn(1)
Dim Shared As Double TwoPi = 8 * Atn(1)
Dim Shared As Double RtoD = 180 / Pi   ' radians * RtoD = degrees
Dim Shared As Double DtoR = Pi / 180   ' degrees * DtoR = radians

dim shared as structure plat(20)
dim shared as agent p, men(50)
dim shared as icon upgrade(10), module
dim shared as particle bits(1000), wraith(4)

dim shared as any ptr bar1, bar2, platnorm(5), rocket, numbers(10), scene, go, me, heatchaser(5), limpetl(2, 2), laser, explode(3), guys(12)
dim shared as any ptr pscr, wall(64),scorebox, pbar, uprow, techtick, upgra(10, 3), tub(10), modulon(5), mb(5), dimmer, hscr, ssmoke, wr(2), wd(10)

dim shared as integer x, xx, y, yy, t, tt, i, ii, curPlatforms, maxPlatforms, minPlatforms, spaceHeld, platforms = 1, motion, ttree(10), tlev
dim shared as integer hWave(30), hSound(30), nextplat, bonus, touch, rl = 0, sclen, scx, scy, pt, rmove, hit, upFlash, tblurb, tbx, tby, ttime
dim shared as integer gameover, menu, intro, flash, flashtime = 5, heldEnter, chaser, chasetime, col, diff, upc, lx, ly, lim, ltim, lf, limprise
dim shared as integer inLava, xplf, xplx, xply, mancount, numMen, tmancount, pHeld, paused, woff, fCount, downHeld, evaMan, holder, mby, md, modd
dim shared as integer uby, ud, upgd, buttons, particles_active, retx, rety, ssz = 0, numEnemies, dflag
dim shared as integer xHeld, zHeld, smulti, ace, mylaser, shield, totaltechs, unlimit, ss, stype, stime = 1, techplus, lfire, mmov, tmmov
dim shared as long score, hiscore, deltatime, lasttime, curtime

dim shared as single pX, pY, gravity = 1, limiTimer, tech, tf, jx, jy, x1, y1, x2, y2, d1, d2

'dim shared as string gfxPath, sfxPath
dim shared as string scor, s

const gfxPath = "./data/graphics/"
const sfxPath = "./data/sound/"
const sx = 1024
const sy = 768
const mode = 20
const depth=32
const pages=2
const TEST = 0
const joystickID = 0

menu = 1
screeninfo retx, rety

upgrade(1).x = 309
upgrade(1).y = 225
upgrade(2).x = 349
upgrade(2).y = 225
upgrade(3).x = 389
upgrade(3).y = 225
upgrade(4).x = 429
upgrade(4).y = 225
upgrade(5).x = 469
upgrade(5).y = 225
upgrade(6).x = 512
upgrade(6).y = 225
upgrade(7).x = 555
upgrade(7).y = 225
upgrade(8).x = 595
upgrade(8).y = 225
upgrade(9).x = 635
upgrade(9).y = 225
upgrade(10).x = 675
upgrade(10).y = 225

declare sub initialiseEnvironment()
declare sub loadAssets()
declare sub getUserInput()
declare sub resetLevel()
declare sub render()
declare sub cleanup()
declare sub update()
declare sub incdiff()
declare sub checkTech()
declare sub switchMode()

initialiseEnvironment()
loadAssets()
lasttime = getTickCount()
do:
    
    timebeginperiod(3)
        wait 1, 1
    timeendperiod(3)

    if rl = 1 then resetLevel()
    curtime = getTickCount()
    deltatime = curtime - lasttime
    lasttime = curtime
    if deltatime < (30 / 1000) then
        screenlock()
        
        render()
    
        screenunlock()
    end if
    
    if intro > 0 then intro -= 1
    limiTimer += 1
    if limiTimer = 8 then
        limiTimer = 0
        getUserInput()
        if gameover = 0 and intro = 0 and menu = 0 and paused = 0 then update()
    end if
    
    if paused = 0 then chasetime -= 1
    if chasetime = 0 then
        chasetime = 128
        chaser += 1
        if chaser = 6 then chaser = 1
    end if
    
    checkTech()
    
    fCount -= 1
    if fCount = 0 then
        fCount = 96
        upFlash = 1 - upFlash
    end if
    
    if paused = 0 then stime -= 1
    if stime = 0 then
        stime = 2
        stype += 1
        if stype = 65 then stype = 1
    end if
    
    if inLava = 1 or hit = 1 then
        if particles_active < 100 and int(rnd * 4) = 3 then
            for tt = 1 to 3
            for t = 1 to 100
                if bits(t).active = 0 then
                    bits(t).active = 1
                    bits(t).x = (p.x - 1) + (rnd * 2)
                    if inLava = 0 then 
                        bits(t).y = p.y -(int(rnd * 8) + 64)
                    else
                        bits(t).y = 758
                    end if
                    bits(t).velY = -int(rnd * 8) + 1
                    bits(t).velX = rnd * 1
                    if bits(t).velX > .4 then bits(t).velX = .4
                    if int(rnd * 2) = 1 then bits(t).velX = -bits(t).velX
                    if int(rnd * 5) = 4 then bits(t).velX = 0
                    bits(t).life = int(rnd * 46) + 5
                    particles_active += 1
                    exit for
                end if
            next t
        next tt
        end if
    end if
        
 
loop until multikey(sc_escape)

cleanup()
screenRes retx, rety, 32

sleep
end

'----------------------------------------------------
'------------------SUBROUTINES-----------------------
'----------------------------------------------------

sub initialiseEnvironment()
    
    'Setup FBsound
    fbs_Set_PlugPath("./plugins/")
    fbs_init()
    
    
    'Set Screen Mode
    ScreenRes sx, sy, depth, pages, ssz
    switchMode()
    View Screen ( 0, 0 )-( sx, sy), RGB(0, 0, 0), RGB(0, 0, 0)
    

    
end sub

sub getUserInput()
    
    GetJoystick(JoystickID, buttons, jx, jy, x1, y1, x2, y2, d1, d2)
    if (buttons and (1 shl 6)) then switchMode()
    if menu = 0 and gameover = 0 and menu = 0 and hit = 0 and inLava = 0 then
        if (multikey(sc_space) or (buttons and (1 shl 0))) and paused = 0 and p.jmp = 0 then 
            spaceHeld += 1 + ttree(7)
            if spaceHeld = 100 then p.lnch = 1
        else
            if spaceHeld > 0 then p.lnch = 1
        end if
        
        if multikey(sc_down) or d2 = 1 or jy = 1 then
            if paused = 0 and p.velY < 20 and p.velY >= -20 and p.jmp = 1 and downHeld = 0 and ttree(3) = 1 then 
                p.velY = 20
                downHeld = 1
                fbs_Play_Sound hSound(22) 
            end if
            if downHeld = 0 and paused = 0 and p.op > 1 and ttree(9) = 1 then
                p.op -= 1
                p.y = plat(p.op).y - 20
                p.velY = 20
                p.x = plat(p.op).x + int(rnd * plat(p.op).w) + 1
                p.jmp = 1
                p.op = 0
                downHeld = 1
                fbs_Play_Sound hSound(15)
            end if
        else
            downHeld = 0
        end if
        if (multikey(sc_up) or d2 = -1 or jy = -1) and paused = 0 and p.jmp = 1 and p.velY >= 0 and p.chg = 50 and ttree(4) = 1 then
            p.chg = 0
            fbs_Play_Sound hSound(21)
            p.velY = -20
        end if
        
        if (multikey(sc_left) or d1 = -1 or jx < -0.5 or x2 < -0.5) and paused = 0 and p.velX > -8 then
            if p.velY < 0 then p.velX -= (0.25 * evaMan)
            if p.velY >= 0 and ttree(8) = 1 then p.velX -= 0.25
            if p.jmp = 0 and ttree(2) = 1 then
                p.x -= 1
            end if
        end if
        if (multikey(sc_right) or d1 = 1 or jx > 0.5 or x2 > 0.5) and paused = 0 and p.velX < 8 then 
            if p.velY < 0 then p.velX += (0.25 * evaMan)
            if p.velY >= 0 and ttree(8) = 1 then p.velX += 0.25
            if p.jmp = 0 and ttree(2) = 1 then
                p.x += 1
            end if
        end if
        if (multikey(sc_j) or (buttons and (1 shl 3))) and paused = 0 and modd > 0 then
            ace = 0
            shield = 0
            mylaser = 0
            techplus = 0
            smulti = 1
        end if
        
        if (multikey(sc_p) or (buttons and (1 shl 7))) then
            if pHeld = 0 then 
                if paused = 1 then paused = 2
                paused = 2 - paused
                fbs_Play_Sound hSound(10)
                pHeld = 1
            end if
        else
            pHeld = 0
        end if
        if (multikey(sc_control) or (buttons and (1 shl 2))) and paused = 0 then
            if mylaser > 0 then lfire = 1
        else
            lfire = 0
        end if
        
    end if
    if (multikey(sc_enter) or (buttons and (1 shl 1))) then
        if menu = 1 and heldEnter = 0 then 
            rl = 1
            heldEnter = 1
            sleep 1
        end if
        if gameover = 1 and heldEnter = 0 then
            gameover = 0
            menu = 1
            heldEnter = 1
            sleep 1
        end if
    else
        heldEnter = 0
    end if
    if ace = 1 then
        if (multikey(sc_z) or (buttons and (1 shl 4))) then
            if zHeld = 0 then
                zHeld = 1
                holder = tlev
retry01:
                tlev -= 1
                if tlev = 0 then tlev = 10
                if ttree(tlev) = 0 then 
                    holder = tlev
                end if
                if holder = tlev then
                holder = 0
                else
                    goto retry01
                end if
            end if
        else
            zHeld = 0
        end if
        if (multikey(sc_x) or (buttons and (1 shl 5))) then
            if xHeld = 0 then
                xHeld = 1
                holder = tlev
retry02:
                tlev += 1
                if tlev = 11 then tlev = 1
                if ttree(tlev) = 0 then 
                    holder = tlev
                end if
                if holder = tlev then
                    holder = 0
                else
                    goto retry02
                end if
            end if
        else
            xHeld = 0
        end if
    end if
    
end sub

sub render()
    
    cls()
    
    if menu = 1 then 
        put (0, 0), me, pset
        draw string (0, 10), "LAST SCORE:" + str(score)
        draw string (0, 32), "HIGH SCORE:" + str(hiscore)
    end if
    if gameover = 1 then put (0, 0), go, pset
    if gameover = 0 and menu = 0 and intro = 0 then
        for y = 1 to 13
            for x = 1 to 16
                put (0 + (x - 1) * 64, (-64 + woff) + (y - 1) * 64), wall(stype), pset
            next x
        next y
        if paused = 2 then 
            put (0, 758), heatchaser(chaser), pset
            put (0, 0), dimmer, alpha, 200
            get (0, 0) - (1023, 767), hscr
        end if
        if paused = 1 then
            put (0, 0), hscr, pset
        else
            if paused = 2 then paused = 1
        end if

        if modd> 0 then
            put (304, 132 + mby), mb(modd), pset
        end if
        if upgd = 1 then put (tbx, tby + uby), tub(tblurb), pset
        if paused = 0 then 
            ttime -= 1
            if ttime = 0 then ud = -1
        end if
        if mylaser > 0 then
            line (312, 152 + mby) - (312 + mylaser, 163 + mby), rgb(255, 0, 0), bf
        end if
        if shield > 0 then
            line (312, 152 + mby) - (312 + shield, 163 + mby), rgb(0, 50, 255), bf
        end if
        if paused < 2 then
            put (304, 220), uprow, pset
            for t = 1 to 10
                if t < 5 or t > 6 then
                    if t = tlev then
                        put (upgrade(t).x, upgrade(t).y), upgra(t, upgrade(t).active + upFlash), pset
                    else
                        put (upgrade(t).x, upgrade(t).y), upgra(t, upgrade(t).active + upgrade(t).coll), pset
                    end if
                else
                    if t = tlev then
                        put (upgrade(t).x, upgrade(t).y), upgra(t, upgrade(t).active + upFlash), trans
                    else
                        put (upgrade(t).x, upgrade(t).y), upgra(t, upgrade(t).active + upgrade(t).coll), trans
                    end if
                end if
            next t
            
            if unlimit = 0 then 
                line (312 + ((tlev * 10) * 4), 290) - (712, 301), rgb(205, 205, 205), bf
            else
                line (312 + ((totaltechs * 10) * 4), 290) - (712, 301), rgb(205, 205, 205), bf
            end if
            if tech > 0.5 and hit = 0 then 
                for tf = 1 to tech
                  put(312 + ((tf - 1) * 4), 290), techtick, pset  
                next tf
            end if
            put (304, 308), pbar, pset
            if ace = 1 then put (307, 310), modulon(5), trans
            if techplus > 0 then put (307, 310), modulon(4), trans
            if shield > 0 then put (307, 310), modulon(3), trans
            if mylaser > 0 then put (307, 310), modulon(2), trans
            if smulti > 1 then put (307, 310), modulon(1), trans
            put (304, 340), scorebox, pset
        end if
    end if
    if menu = 0 and intro = 0 and paused < 2 then
        for t = 1 to sclen
            scor = str(score)
            s = mid(scor, t, 1)
            ii = val(s)
            put ((sx - (sclen * 40)) / 2 + (t - 1) * 40, scy), numbers(ii + 1), trans
        next t
    end if
    
    if gameover = 0 and menu = 0 and intro = 0 then 
        if spaceHeld > 0 and hit = 0 then
            for t = 1 to spaceHeld
                line (453 - (t - 1), 311) - (453 - (t - 1), 336), rgb(30, 180, 30)
                line (569 + (t - 1), 311) - (569 + (t - 1), 336), rgb(30, 180, 30)
            next t
        end if
        if numEnemies > 0 and paused = 0 then
            for t = 1 to 4
                if wraith(t).active > 0 then 
                    if wraith(t).active < 3 then put (wraith(t).x, wraith(t).y), wr(wraith(t).active), trans
                    if wraith(t).active = 3 then put (wraith(t).x, wraith(t).y), wd(wraith(t).frm), trans
                end if
            next t
        end if
        if platforms > 0 and paused = 0 then
            for t = 1 to 6
                if plat(t).y + 63 >= 0 then
                    for tt = 1 to plat(t).sects
                        if plat(t).m = 1 then 
                            put(plat(t).x + (tt - 1) * 32, plat(t).y), platnorm(5), trans
                        else
                            put(plat(t).x + (tt - 1) * 32, plat(t).y), platnorm(1), trans
                        end if
                    next tt
                end if
            next t
        end if
        
        if paused = 0 then
            for t = 1 to 50 
                if men(t).active = 1 and men(t).y + 32 >= 0 then
                    if men(t).res = 1 then put (men(t).x, men(t).y), guys(2), trans
                    if men(t).res = 0 then put (men(t).x, men(t).y), guys(1 + men(t).frm), trans
                end if
                if men(t).active = 2 or men(t).active = 3 then put (men(t).x, men(t).y), guys(men(t).diefrm), trans
            next t
            if p.velY < -10 then 
                randomize
                col = 240
                line (p.x - 4, p.y) - (p.x + 4, p.y + 64), rgb(col, col, col - 150), BF
            end if
        end if
        if module.y + 27 >= 0 and module.active = 1 and paused = 0 then put (module.x, module.y), modulon(module.f), trans
        if paused = 0 then
            put (p.x - 32, p.y - 80), rocket, trans
            if mylaser > 0 and lfire = 1 then
                line (p.x - 2, p.y - 82) - (p.x - 2, 0), rgb(250, 30, 30)
                line (p.x - 1, p.y - 80) - (p.x - 1, 0), rgb(200, 0, 0)
                line (p.x, p.y - 80) - (p.x, 0), rgb(220, 0, 0)
                line (p.x + 1, p.y - 80) - (p.x + 1, 0), rgb(200, 0, 0)
                line (p.x + 2, p.y - 82) - (p.x + 2, 0), rgb(250, 30, 30)
            end if
            if particles_active > 0 then
                for t = 1 to 100
                    if bits(t).active = 1 then
                        put (bits(t).x, bits(t).y), ssmoke, trans
                    end if
                next t
            end if
        end if
        if paused = 0 then put (0, 758), heatchaser(chaser), pset
        put (0, 0), bar1, pset
        put (1014, 0), bar2, pset
        if lim = 1 and paused = 0 then
            put (lx, ly), limpetl(1, 1 + lf), trans
            put (1023 - 31, ly), limpetl(2, 1 + lf), trans
            if lf = 1 then
                for t = 1 to 30
                    put(32 + (t - 1) * 32, ly + 5), laser, pset
                next t
            end if
        end if
        if xplf > 0 and paused = 0 then
            put (p.x + xplx, p.y - xply), explode(xplf), trans
        end if
        if paused = 1 then put ((sx - 494) / 2, 0), pscr, trans
    end if
    
end sub

sub update()
    
    if motion = 1 then
        p.y += 3
        pY += 3
        for t = 1 to 20
            plat(t).y += 3
        next t
        for t = 1 to 50
            if men(t).active = 1 or men(t).active = 3 then
                men(t).y += 3
            end if
        next t
        for t = 1 to 4
            if wraith(t).active = 1 then
                wraith(t).y += 3
            end if
        next t
        woff += 3
        if lim = 1 then ly += 3
        score += 3 * smulti
        if module.active = 1 then module.y += 3
    end if
    
    if p.lnch = 1 then
        p.velY = -((spaceHeld / 8) + 26)
        p.lnch = 0
        spaceHeld = 0
        p.jmp = 1
        p.op = 0
        motion = 1
        fbs_Play_Sound hsound(1)
    end if
    if p.jmp = 1 then 
        p.y += p.velY 
        if inLava = 0 then p.velY += gravity
        if p.velY > 20 then p.velY = 20
        if p.op > 0 then p.velY = 0
    end if
    
    p.x += p.velX
    
    if p.x < 21 then
        p.x = 21
        if p.velX < 0 then 
            p.velX = -p.velX
            fbs_Play_Sound hSound(3)
        end if
    end if
    if p.x > sx - 32 then
        p.x = sx - 32
        if p.velX > 0 then
            p.velX = -p.velX
            fbs_Play_Sound hSound(3)
        end if
    end if
        
    if p.y < 100 then 
        p.y += 4
        pY += 4
        for t = 1 to 20
            plat(t).y += 4
        next t
        for t = 1 to 50
            if men(t).active = 1 or men(t).active = 3 then
                men(t).y += 4
            end if
        next t
        for t = 1 to 4
            if wraith(t).active = 1 then
                wraith(t).y += 4
            end if
        next t
        woff += 4
        if lim = 1 then ly += 4
        if module.active = 1 then module.y += 3
        score += 4 * smulti
    end if
    
    if particles_active > 0 then
        for t = 1 to 100
            if bits(t).active = 1 then
                bits(t).life -= 1
                if bits(t).life = 0 then
                    bits(t).active = 0
                    particles_active -= 1
                else
                    bits(t).x += bits(t).velX
                    bits(t).y += bits(t).velY
                end if
            end if
        next t
    end if
    
    if platforms > 0 then 
        for t = 1 to platforms
            if p.velY > 0 then
                for y = 1 to p.velY
                    for x = p.x - 32 to p.x + 32
                        if x >= plat(t).x and x <= plat(t).x + plat(t).w and p.y + y >= plat(t).y and p.y + y <= plat(t).y + 4 and p.jmp = 1 and hit = 0 then
                            p.y = plat(t).y
                            if p.velY >= 16 and ttree(1) = 0 then 
                                p.velY = -10
                                fbs_Play_Sound hSound(11)
                            else
                                p.jmp = 0
                                p.lnch = 0
                                p.op = t
                                fbs_Play_Sound hSound(11)
                                if ttree(6) = 1 then p.velX = 0
                                if p.op = nextplat then
                                    fbs_Play_Sound hSound(4)
                                    pt += 1
                                    nextplat += 1
                                    plat(t).v = 2
                                    bonus += 1
                                else
                                    if plat(t).v = 1 then
                                        pt += 1
                                        if p.op > nextplat then nextplat = t + 1
                                    end if
                                    bonus = 1
                                    for tt = 20 to 1 step -1
                                        if plat(tt).v = 2 or tt <= p.op then plat(tt).v = 3
                                    next tt
                                end if
                                if plat(t).v = 1 then score += (pt * bonus) * smulti
                                p.velY = 0
                            end if
                        end if
                    next x
                next y
            end if
            if t = p.op then
                if p.x - 33 > plat(t).x + plat(t).w or p.x + 33 < plat(t).x then
                    p.op = 0
                    p.y += 2
                    p.jmp = 1
                end if
            end if
            if plat(t).m = 1 then
                plat(t).x += plat(t).velX
                if p.op = t then p.x += plat(t).velX
                if plat(t).velX > 0 then
                    if plat(t).x + (plat(t).w - 1) > 1013 then
                        plat(t).x = 1013 - plat(t).w
                        plat(t).velX = -plat(t).velX
                    end if
                end if
                if plat(t).velX < 0 then
                    if plat(t).x < 10 then
                        plat(t).x = 10
                        plat(t).velX = -plat(t).velX
                    end if
                end if
            end if 
        next t
    end if
    
    pY = plat(20).y
    if p.y >= 760 then
        spaceHeld = 0
        if inLava = 0 then fbs_Play_Sound hSound(5)
        if hit = 1 then 
            inLava = 1
            motion = 0
            p.velY = 1
            p.velX = 0
        else
            if ttree(10) = 1 then
                ttree(10) = -1
                spaceHeld = 100
                p.lnch = 1
                p.y = 758
            else
                inLava = 1
                motion = 0
                p.jmp = 1
                p.velX = 0
                p.velY = 1
                hit = 0
                if hit = 1 then 
                    fbs_Play_Sound hSound(7)
                    xplf = int(rnd * 3) + 1
                    xplx = int(rnd * 20) - 10
                    xply = int(rnd * 10) + 1
                    p.op = 0
                end if
            end if
        end if
    end if
    if p.y - 80 > sy - 1 then 
        gameover = 1
        if score > hiscore then hiscore = score
    end if
    
    flashtime -= 1
    if flashtime = 0 then
        flashtime = 24
        flash = 3 - flash
    end if
    
    if p.jmp = 0 and p.velX > 0 then p.velX -= 0.25
    if p.jmp = 0 and p.velX < 0 then p.velX += 0.25
    
    scor = str(score)
    sclen = len(scor)
    scx = (sx - (sclen * 70)) / 2
    
loop2:
    touch = 0
    if plat(1).y >= 758 then
        upc += 1
        touch = 1
        for t = 2 to 20
            plat(t - 1).x = plat(t).x
            plat(t - 1).y = plat(t).y
            plat(t - 1).w = plat(t).w
            plat(t - 1).h = plat(t).h
            plat(t - 1).life = plat(t).life
            plat(t - 1).m = plat(t).m
            plat(t - 1).velX = plat(t).velX
            plat(t - 1).velY = plat(t).velY
            plat(t - 1).tick = plat(t).tick
            plat(t - 1).sects = plat(t).sects
            plat(t - 1).v = plat(t).v
            for tt = 1 to 50
                if men(tt).active = 1 then
                    if men(tt).op = t then men(tt).op -= 1
                end if
            next tt
        next t
        plat(20).y = plat(19).y - (int(rnd * (250)) + 150)
        plat(20).w = int(rnd * (maxPlatforms - minPlatforms)) + (minPlatforms + 1)
        plat(20).sects = plat(20).w
        plat(20).v = 1
        plat(20).m = 0
        plat(20).velX = 0
        plat(20).velY = 0
        plat(20).w *= 32
        plat(20).h= 16
        plat(20).x = int(rnd * (sx - (plat(20).w + 128))) + 64     
        randomize
        rmove = int(rnd * 3) + 1
        if rmove = 3 and plat(20).sects <= minPlatforms + 2 then
            plat(20).m = 1
            while plat(20).velX = 0
                plat(20).velX = int(rnd * 3) - 1
            wend
        end if
        if numMen < 47 and plat(20).m = 0 then
            randomize
            if int(rnd * 10) +1 >= 7 then
                tmancount = int(rnd * 3) + 1
                for tt = 1 to tmancount
                    mancount = 0
                    for t = 1 to 50
                        if men(t).active = 0 and mancount = 0 then
                            numMen += 1
                            men(t).active = 1
                            men(t).op = 20
                            men(t).res = 0
                            men(t).frm = 0
                            men(t).y = plat(20).y - 32
                            men(t).x = plat(20).x + int(rnd * (plat(20).w - 16))
                            mancount = 1
                        end if
                    next t
                next tt
            end if
        end if
        pY = plat(20).y
        if p.op > 0 then p.op -= 1
        if nextplat > 0 then nextplat -= 1
        rmove = 0
    end if
    if touch = 1 then goto loop2
    if upc = diff * 250 then incdiff()
    if lim > 0 then
        ly += limprise
        if ly >= 743 then limprise = -6
        if ly <= -11 then lim = 0
        ltim -= 1
        if ltim = 0 then
            lf = 1 - lf
            if lf = 1 then fbs_Play_Sound hSound(6)
            ltim = 100 - (50 * lf)
        end if
    end if
    if lim = 0 and diff >= 1 and motion = 1 and upc >= 20 then
        randomize
        if int(rnd * 10000) + 1 >= (9997 - (10 * (diff - 1))) then
            lim = 1
            ltim = 50
            lx = 0
            ly = -11
            limprise = 1
        end if
    end if
    
    if lim = 1 and lf = 1 then 
        'hit = 0
        dflag = 0
        for t = 5 to 17
            if ly + t > p.y - 80 and ly + t < p.y and hit = 0 then
                if shield > 0 then 
                    if dflag < 6 then
                        shield -= 1
                        dflag += 1
                        if shield < 0 then shield = 0
                    end if
                else
                    hit = 1
                    fbs_Play_sound hSound(7)
                    xplf = int(rnd * 3) + 1
                    xplx = int(rnd * 40) - 20
                    xply = int(rnd * 75) + 1
                    p.op = 0
                    if inLava = 0 then p.velY = -8
                    p.jmp = 1
                end if
            end if
            for tt = 1 to 50
                if men(tt).active = 1 and men(tt).res = 0 then
                    if ly + t > men(tt).y and ly + t < men(tt).y + 32 then
                        men(tt).active = 3
                        men(tt).die = 50
                        men(tt).diefrm = 7
                        fbs_Play_Sound hSound(16)
                        fbs_Play_Sound hSound(9)
                        if techplus = 0 then tech -= 0.2
                    end if
                end if
            next tt
            for tt = 1 to 4
                if wraith(tt).active = 1 and wraith(tt).velY > - 4 then
                    if ly + t > wraith(tt).y and ly + t < wraith(tt).y + 64 then
                        wraith(tt).velX = 0
                        wraith(tt).velY = -20
                        wraith(tt).active = 3
                        wraith(tt).frm = 1
                        score += 500 * smulti
                        fbs_Play_Sound hSound(18)
                    end if
                end if
            next tt
        next t
    end if
    if hit = 1 then
        if int(rnd * 20) + 1 >= 16 then 
            fbs_play_Sound hSound(7)
            xplf = int(rnd * 3) + 1
            xplx = int(rnd * 40) - 20
            xply = int(rnd * 50) + 1
        end if
    end if
    
    for t = 1 to 50
        if men(t).active = 2 or men(t).active = 3 then
            men(t).die -= 1
            if men(t).die = 0 then
                men(t).diefrm = 0
                men(t).active = 0
                numMen -= 1
                if techplus = 0 then tech -= 0.2
            else
                men(t).diefrm += 1
                if men(t).diefrm = 13 then men(t).diefrm = 7
            end if
        end if
        if men(t).active = 1 then
            if men(t).res = 1 then
                men(t).y -= 20
                if men(t).y < -30 then
                    men(t).res = 0
                    men(t).active = 0
                    numMen -= 1
                end if
            end if
            if men(t).op = p.op and men(t).res = 0 and p.op > 0 then
                if ttree(5) = 1 then
                    men(t).res = 1
                    men(t).velY = -20
                    men(t).frm = 0
                    men(t).op = -1
                    score += 50 * smulti
                    tech += 0.5
                    tech += techplus / 2
                    fbs_Play_Sound hSound(8)
                else
                    if p.x > men(t).x then 
                        men(t).x += 2
                        men(t).frm = 2 + mmov
                    end if
            
                    if p.x < men(t).x then 
                        men(t).x -= 2
                        men(t).frm = 3 + mmov
                    end if
                    if tmmov = 0 then
                        mmov = 2 - mmov
                        tmmov = 16
                    else
                        tmmov -= 1
                    end if
                end if
            else
                men(t).frm = 0
            end if
        end if

        if men(t).active = 1 then
            if men(t).y + 31 >= 758 then
                men(t).active = 2
                men(t).die = 50
                men(t).diefrm = 7
                fbs_Play_Sound hSound(16)
                fbs_Play_Sound hSound(9)
            end if
            if ((men(t).x <= p.x and (men(t).frm = 3 or men(t).frm = 5)) or (men(t).x + 11 >= p.x and (men(t).frm = 2 or men(t).frm = 4))) and men(t).res = 0 and men(t).op = p.op then
                men(t).res = 1
                men(t).frm = 0
                men(t).velY = -20
                score += 50 * smulti
                men(t).op = 0
                tech += 0.5
                tech += techplus / 2
                fbs_Play_Sound hSound(8)
            end if
        end if
    next t
    
    if woff >= 64 then woff -= 64
    if p.chg < 50 then p.chg += 1
    
    if module.active = 0 and paused = 0 then
     '   randomize
        if int(rnd * 2000) + 1 >= 1999 and motion > 0 then
            module.active = 1
            module.y = -30
         '   randomize
            module.x = 21 + int(rnd * (1024 - 98))
            holder = int(rnd * 10) + 1
            module.f = 5
            if holder < 10 then module.f -= 1
            if holder < 8 then module.f -= 1
            if holder < 6 then module.f -= 1
            if holder < 4 then module.f -= 1
        end if
        if module.active = 0 then
            if int(rnd * 1000) = 999 then
                module.active = 1
                module.y = -30
             '   randomize
                module.x = 21 + int(rnd * (1024 - 98))
                if int(rnd * 2) = 1 then
                    module.f = 3
                else
                    module.f = 2
                end if
            end if
        end if
    else
        if module.x + 43 >= p.x - 32 and module.x <= p.x + 32 and module.y + 27 >= p.y - 79 and module.y <= p.y and hit = 0 and modd = 0 then
            ace = 0
            shield = 0
            mylaser = 0
            techplus = 0
            smulti = 1
            score += 25
            mby = 88
            md =- 1
            modd = module.f
            fbs_Play_Sound hSound(20)
            if module.f = 5 then ace = 1
            if module.f = 4 then techplus = 1
            if module.f = 3 then shield = 400
            if module.f = 2 then mylaser = 400
            if module.f = 1 then 
                smulti = 2
                score += 1000
            end if
            if techplus = 1 then
                holder = 0
                tech = int(tech)
                for t = 5 to 100 step 5
                    if tech < t and holder = 0 then
                        tech = t
                        holder = 1
                    end if
                next t
            end if
            module.active = 0
            module.x = 0
            module.y = 0
            module.f = 0
        end if
        module.y += module.velY
        if module.y >= 758 then 
            module.x = 0
            module.y = 0
            module.active = 0
            module.f = 0
        end if
    end if
    
    if mylaser > 0 and lfire = 1 then 
        fbs_Play_Sound hSound(19)
        mylaser -= 1
    end if
    
    if modd > 0 then
        if mby > 0 and md = -1 then 
            if mby = 88 then fbs_Play_Sound hSound(2)
            mby -= 1
        end if
        if mby < 88 and md = 1 then 
            if mby = 0 then fbs_Play_Sound hSound(2)
            mby += 1
        end if
        if mby = 88 then modd = 0
        if ace = 0 and shield = 0 and mylaser = 0 and techplus = 0 and smulti = 1 then md = 1
    end if
    
    if upgd = 1 then
        if uby < 88 and ud = 1 then 
            if uby = 0 then fbs_Play_Sound hSound(2)
            uby += 1
        end if
        if uby > 0 and ud = -1 then 
            if uby = 88 then fbs_Play_Sound hSound(2)
            uby -= 1
        end if
        if uby = 0 then upgd = 0
    end if
    
    if numEnemies > 0 then
        for t = 1 to 4
            if wraith(t).active > 0 then
                if wraith(t).spd = 0 then
                    if wraith(t).active = 2 then wraith(t).active = 1
                    wraith(t).spd = 1
                    if wraith(t).chg > 0 and ((wraith(t).velY < 0 and wraith(t).y <= wraith(t).chg) or (wraith(t).velY > 0 and wraith(t).y >= wraith(t).chg)) then 
                        wraith(t).velX = -1
                        if wraith(t).destx > wraith(t).x then wraith(t).velX = 1
                        wraith(t).x += wraith(t).velX
                        if wraith(t).x = wraith(t).destx then
                            wraith(t).velX = 0
                            wraith(t).destx = 0
                            wraith(t).chg = 0
                            wraith(t).velY = wraith(t).uvy
                        end if
                    else
                        wraith(t).y += wraith(t).velY
                    end if
                    if wraith(t).y > sy - 1 or wraith(t).y + 63 < -64 then
                        wraith(t).velX = 0
                        numEnemies -= 1
                        wraith(t).active = 0
                        wraith(t).frm = 0
                    end if
                    if wraith(t).frm > 0 then wraith(t).frm += 1
                    if wraith(t).frm = 11 then
                        wraith(t).frm = 0
                        wraith(t).active = 0
                        numEnemies -= 1
                    end if
                else
                    wraith(t).spd -= 1
                end if
                if p.x  - 32 <= wraith(t).x + 63 and p.x + 32 >= wraith(t).x and p.y >= wraith(t).y and p.y - 79 <= wraith(t).y + 63 then
                    p.op = 0
                    p.jmp = 1
                    p.y = wraith(t).y - 1
                    p.velY = -10
                    p.velX = -10
                    fbs_Play_Sound hSound(23)
                    if p.x >= wraith(t).x + 32 then p.velX = -p.velX
                end if
                if lfire = 1 and wraith(t).spd >0 then
                    if p.x - 2 <= wraith(t).x + 63 and p.x + 2 >= wraith(t).x and p.y - 80 > wraith(t).y and wraith(t).active = 1 then
                        if wraith(t).life > 0 then 
                            wraith(t).life -= 1
                            wraith(t).active = 2
                            wraith(t).y -= 8
                            score += 20 * smulti
                            fbs_Play_Sound hSound(17)
                            wraith(t).spd = 4
                        else
                            wraith(t).velY = -20
                            wraith(t).active = 3
                            wraith(t).frm =  1
                            score += 250 * smulti
                            fbs_Play_Sound hSound(18)
                        end if
                    end if
                end if
            end if
        next t
    end if
    if numEnemies < 4 and int(rnd * 1000) + 1 > (997 - diff) and motion > 0 then
        for t = 1 to 4
            if wraith(t).active = 0 then
                numEnemies += 1
                wraith(t).active =1
                wraith(t).life = 3
                wraith(t).x = int(rnd * (sx - 192)) + 128
                wraith(t).y = -64
                if int(rnd * 2) = 1 then wraith(t).y = 758
                if wraith(t).y = 758 then
                    wraith(t).velY = -2
                else
                    wraith(t).velY = 2
                end if
                wraith(t).uvy  = wraith(t).velY
                wraith(t).chg = 0
                wraith(t).destx = 0
                if int(rnd * 3) + 1 = 3 then 
                    wraith(t).chg = int(rnd * 568) + 100
                    wraith(t).destx = sx - wraith(t).x
                end if
                wraith(t).spd = 1
                exit for
            end if
        next t
    end if
    
    if hit = 1 or inLava = 1 then lfire = 0
end sub

sub cleanup()
    
    for t = 1 to 30
        fbs_Destroy_Sound(@hSound(t))
    next t
    
    for t = 1 to 30
        fbs_Destroy_Wave(@hWave(t))
    next t
    
    for t = 1 to 10
        imagedestroy wd(t)
    next t
        
    for t = 1 to 3
        imagedestroy explode(t)
    next t
        
    for y = 1 to 2
        for x = 1 to 2
            imagedestroy limpetl(x, y)
        next x
    next y
        
    for t = 1 to 5
        imagedestroy heatchaser(t)
    next t
        
    for t = 1 to 30
        imagedestroy numbers(t)
    next t
    
    for t = 1 to 5
        imagedestroy platnorm(t)
    next t
    
    for t = 1 to 12
        imagedestroy guys(t)
    next t
    
    for t = 1 to 10
        for tt = 1 to 3
            imagedestroy upgra(t, tt)
        next tt
    next t    
      
    for t = 1 to 64
        imagedestroy wall(t)
    next t
    
    for t = 1 to 5
        imagedestroy mb(t)
    next t
    
    for t = 1 to 10
        imagedestroy tub(t)
    next t
    
    for t = 1 to 5
        imagedestroy modulon(t)
    next t
    for t = 1 to 2
        imagedestroy wr(t)
    next t
    imagedestroy ssmoke
    imagedestroy dimmer
    imagedestroy techtick
    imagedestroy uprow
    imagedestroy pbar
    imagedestroy scorebox
    imagedestroy pscr
    imagedestroy laser
    imagedestroy me
    imagedestroy go
    imagedestroy scene
    imagedestroy rocket
    imagedestroy bar2    
    imagedestroy bar1
    imagedestroy hscr
    
end sub

sub loadAssets()
    
    hscr = imagecreate(1024, 768)
    bar1 = imagecreate(10, 768)
    bload gfxPath + "sideleft.bmp", bar1
    bar2 = imagecreate(10, 768)
    bload gfxPath + "sideright.bmp", bar2
    rocket = imagecreate(64, 80, rgb(255, 0, 255))
    bload gfxPath + "rocket.bmp", rocket
    scene = imagecreate(1024, 768)
    bload gfxPath + "scene.bmp", scene
    go = imagecreate(1024, 768)
    bload gfxPath +"goscr.bmp", go
    me = imagecreate(1024, 768)
    bload gfxPath + "menscr.bmp", me
    laser = imagecreate(32, 12)
    bload gfxPath + "laser.bmp", laser
    pscr = imagecreate(494, 114, rgb(255, 0, 255))
    bload gfxPath + "pause2.bmp", pscr
    scorebox = imagecreate(416, 88)
    bload gfxPath + "sb1.bmp", scorebox
    pbar = imagecreate(416, 32)
    bload gfxPath + "pb1.bmp", pbar
    uprow = imagecreate(416, 88)
    bload gfxPath + "uprow.bmp", uprow
    techtick = imagecreate(4, 11)
    bload gfxPath + "techtick.bmp", techtick
    dimmer = imagecreate(1024, 768)
    bload gfxPath + "dim.bmp", dimmer
    ssmoke = imagecreate(13, 16, rgb(255, 0, 255))
    bload gfxPath + "smoke1.bmp", ssmoke
    wr(1) = imagecreate(64, 64, rgb(255, 0, 255))
    wr(2) = imagecreate(64, 64, rgb(255, 0, 255))
    bload gfxPath + "wraith.bmp", wr(1)
    bload gfxPath + "wraith2.bmp", wr(2)
    cls()
    
    bload gfxPath + "module.bmp", 0
    for t = 1 to 5
        modulon(t) = imagecreate(44, 28, rgb(255, 0, 255))
        get(0 + (t - 1) * 44, 0) - (43 + (t - 1) * 44, 27), modulon(t)
    next t
    cls()
    
    for t = 1 to 10
        tub(t) = imagecreate(416, 73)
        bload gfxPath + "tb" + str(t) + ".bmp", tub(t)
    next t
    
    for t = 1 to 5
        mb(t) = imagecreate(416, 88)
        bload gfxPath + "mb" + str(t) + ".bmp", mb(t)
    next t
    cls()
        
    bload gfxPath + "wall64.bmp", 0
    holder = 1
    for tt = 1 to 4
        for t = 1 to 16
            wall(holder) = imagecreate(64, 64)
            get (0 + (t - 1) * 64, 0 + (tt - 1) * 64) - (63 + (t - 1) * 64, 63 + (tt - 1) * 64), wall(holder)
            holder += 1
        next t
    next tt
    cls()
    
    for t = 1 to 10
        bload gfxPath + "upgrade" + str(t) + ".bmp", 0
        for tt = 1 to 3
            if t < 5 or t > 6 then
                upgra(t, tt) = imagecreate(40, 54)
                get (0 + (tt - 1) * 40, 0) - (39 + (tt - 1) * 40, 53), upgra(t, tt)
            else
                upgra(t, tt) = imagecreate(43, 54, rgb(255, 0, 255))
                get (0 + (tt - 1) * 43, 0) - (42 + (tt - 1) * 43, 53), upgra(t, tt)
            end if
        next tt
    next t
    cls()
    
    bload gfxPath + "newguys.bmp", 0
    for t = 1 to 12
        guys(t) = imagecreate(16, 32, rgb(255, 0, 255))
        get (0 + (t - 1) * 16, 0) - (15 + (t - 1) * 16, 31), guys(t)
    next t
    cls()
    
    bload gfxPath + "platform4.bmp", 0
    for t = 1 to 5
        platnorm(t) = imagecreate(32, 48, rgb(255, 0, 255))
        get (0 + (t - 1) * 32, 0) - (31 + (t - 1) * 32, 47), platnorm(t)
    next t
    cls()
    
    bload gfxPath + "numbers1.bmp", 0
    for t = 1 to 10
        numbers(t) = imagecreate(40, 72, rgb(255, 0, 255))
        get (0 + (t - 1) * 40, 0) - (39 + (t - 1) * 40, 71), numbers(t)
    next t
    cls()
    
    bload gfxPath + "heatchaseT3.bmp", 0
    for t = 1 to 5
        heatchaser(t) = imagecreate(1024, 10)
        get (0, 0 + (t - 1) * 10) - (1023, 9 + (t - 1) * 10), heatchaser(t)
    next t
    cls()
    
    bload gfxPath + "lll.bmp", 0
    for t = 1 to 2
        limpetl(1, t) = imagecreate(32, 22)
        get(0 + (t - 1) * 32, 0) - (31 + (t - 1) * 32, 21), limpetl(1, t)
    next t
    cls()
    
    bload gfxPath + "llr.bmp", 0
    for t = 1 to 2
        limpetl(2, t) = imagecreate(32, 22)
        get(0 + (t - 1) * 32, 0) - (31 + (t - 1) * 32, 21), limpetl(2, t)
    next t
    cls()
    
    bload gfxPath + "explosions.bmp", 0
    for t = 1 to 3
        explode(t) = imagecreate(20, 20, rgb(255, 0, 255))
        get (0 + (t - 1) * 20, 0) - (19 + (t - 1) * 20, 19), explode(t)
    next t
    cls()
    
    bload gfxPath + "wraithdeath.bmp", 0
    for t = 1 to 10
        wd(t) = imagecreate(64, 64, rgb(255, 0, 255))
        get (0 + (t - 1) * 64, 0) - (63 + (t - 1) * 64, 63), wd(t)
    next t
    cls()
        
    fbs_Load_WAVFile(sfxPath + "launch.wav", @hWave(1))
    fbs_Load_WAVFile(sfxPath + "mecha.wav", @hWave(2))
    fbs_Load_WAVFile(sfxPath + "sidehit.wav", @hWave(3))
    fbs_Load_WAVFile(sfxPath + "bonusup.wav", @hWave(4))
    fbs_Load_WAVFile(sfxPath + "crash2.wav", @hWave(5))
    fbs_Load_WAVFile(sfxPath + "laser4.wav", @hWave(6))
    fbs_Load_WAVFile(sfxPath + "explosion.wav", @hWave(7))
    fbs_Load_WAVFile(sfxPath + "beamup.wav", @hWave(8))
    fbs_Load_WAVFile(sfxPath + "flame.wav", @hWave(9))
    fbs_Load_WAVFile(sfxPath + "pause.wav", @hWave(10))
    fbs_Load_WAVFile(sfxPath + "land.wav", @hWave(11))
    fbs_Load_WAVFile(sfxPath + "scream2.wav", @hWave(12))
    fbs_Load_WAVFile(sfxPath + "hop.wav", @hWave(13))
    fbs_Load_WAVFile(sfxPath + "techup.wav", @hWave(14))
    fbs_Load_WAVFile(sfxPath + "teledown.wav", @hWave(15))
    fbs_Load_WAVFile(sfxPath + "manyelp.wav", @hWave(16))
    fbs_Load_WAVFile(sfxPath + "scream.wav", @hWave(17))
    fbs_Load_WAVFile(sfxPath + "scream3.wav", @hWave(18))
    fbs_Load_WAVFile(sfxPath + "laser5.wav", @hWave(19))
    fbs_Load_WAVFile(sfxPath + "modget.wav", @hWave(20))
    fbs_Load_WAVFile(sfxPath + "rasp.wav", @hWave(21))
    fbs_Load_WAVFile(sfxPath + "drasp.wav", @hWave(22))
    fbs_Load_WAVFile(sfxPath + "spring.wav", @hWave(23))
    fbs_Create_Sound(hWave(1), @hSound(1))
    fbs_Create_Sound(hWave(2), @hSound(2))
    fbs_Create_Sound(hWave(3), @hSound(3))
    fbs_Create_Sound(hWave(4), @hSound(4))
    fbs_Create_Sound(hWave(5), @hSound(5))
    fbs_Create_Sound(hWave(6), @hSound(6))
    fbs_Create_Sound(hWave(7), @hSound(7))
    fbs_Create_Sound(hWave(8), @hSound(8))
    fbs_Create_Sound(hWave(9), @hSound(9))
    fbs_Create_Sound(hWave(10), @hSound(10))
    fbs_Create_Sound(hWave(11), @hSound(11))
    fbs_Create_Sound(hWave(12), @hSound(12))
    fbs_Create_Sound(hWave(13), @hSound(13))
    fbs_Create_Sound(hWave(14), @hSound(14))
    fbs_Create_Sound(hWave(15), @hSound(15))
    fbs_Create_Sound(hWave(16), @hSound(16))
    fbs_Create_Sound(hWave(17), @hSound(17))
    fbs_Create_Sound(hWave(18), @hSound(18))
    fbs_Create_Sound(hWave(19), @hSound(19))
    fbs_Create_Sound(hWave(20), @hSound(20))
    fbs_Create_Sound(hWave(21), @hSound(21))
    fbs_Create_Sound(hWave(22), @hSound(22))
    fbs_Create_Sound(hWave(23), @hSound(23))
    
end sub

sub resetLevel()
    
    for t = 1 to 1000
        bits(t).active = 0
    next t
    particles_active = 0
    
    platforms = 0
    motion = 0
    numEnemies = 0
    
    for t = 1 to 4
        wraith(t).active = 0
    next t
    
    p.x = sx / 2
    p.y = sy - 31
    p.w = 65
    p.h = 80
    p.jmp = 1
    p.lnch = 0
    p.velX = 0
    p.velY = 0
    p.chg = 0
    spaceHeld = 0
    tbx = 304
    tby = 340
    
    plat(1).x = 64
    plat(1).w = 28 * 32
    plat(1).sects = 28
    plat(1).h = 16
    plat(1).v = 3
    plat(1).y = sY - (plat(1).h + 1) - 16
    plat(1).m = 0
    plat(1).life = 0
    plat(1).velX = 0
    plat(1).velY = 0
    plat(1).tick = 0
    
    pY = plat(1).y
    
    for t = 2 to 20
        plat(t).x = 0
        plat(t).w = 0
        plat(t).sects = 0
        plat(t).h = 0
        plat(t).v = 0
        plat(t).y = 0
        plat(t).m = 0
        plat(t).life = 0
        plat(t).velX = 0
        plat(t).velY = 0
        plat(t).tick = 0
    next t
    
    for t = 1 to 50
        men(t).active = 0
    next t
    
    minPlatforms = 8
    maxPlatforms = 16
    nextplat = 1
    bonus = 0
    score = 0
    sclen = 1
    scx = (sy - 40) / 2
    scy = 348
    pt = 0
    platforms = 1
    randomize
    numMen = 0
    for t = 1 to 50
        men(t).active = 0
    next t    
    for t = 2 to 20
        if t > 2 then plat(t).y = pY - (int(rnd * (250)) + 150)
        if t = 2 then plat(t).y = pY - 400
        plat(t).w = int(rnd * (maxPlatforms - minPlatforms)) + (minPlatforms + 1)
        plat(t).sects = plat(t).w
        plat(t).w *= 32
        plat(t).h = 16
        plat(t).v = 1
        plat(t).x = int(rnd * (sx - (plat(t).w + 128))) + 64
        pY = plat(t).y
        platforms += 1
        
        if numMen < 47 then
            randomize
            if int(rnd * 10) +1 >= 7 then
                tmancount = int(rnd * 3) + 1
                for tt = 1 to tmancount
                    numMen += 1
                    men(numMen).active = 1
                    men(numMen).op = t
                    men(numMen).frm = 0
                    men(numMen).die = 0
                    men(numMen).y = plat(t).y - 32
                    men(numMen).x = plat(t).x + int(rnd * (plat(t).w - 16))
                next tt
            end if
        end if        
    next t 
    
    rl = 0
    menu = 0
    flash = 0
    flashtime = 10
    chaser = 1
    chasetime = 128
    diff = 1
    upc = 0
    lf = 0
    ltim = 0
    lim = 0
    hit = 0
    inLava = 0
    xplf = 0
    xplx = 0
    xply = 0
    mancount = 0
    woff = 0
    tech = 0
    upFlash = 0
    fCount = 96
    tblurb = 0
    ttime = 0
    ace = 0
    smulti = 1
    shield = 0
    mylaser = 0
    techplus = 0
    totaltechs = 1
    unlimit = 0
    ss = 1
    stype = 1
    
    modd = 0
    mby = 0
    md = 0
    
    module.x = 0
    module.y = 0
    module.f = 0
    module.active = 1
    module.velY = .5
    
    for t = 1 to 10
        upgrade(t).coll = TEST * 2
        upgrade(t).active = 1
        ttree(t) = upgrade(t).coll / 2
    next t
    if ttree(10) = 1 then
        tlev = 0
    else 
        tlev = 1
    end if
    if tlev = 0 then 
        tech = 100
    else
        tech = 0
    end if
    evaMan = 1
    if ttree(8) = 1 then evaMan = 4
    
end sub

sub incdiff()
    
    if maxPlatforms > minPlatforms + 3 then
        maxPlatforms -= 1
        if maxPlatforms = minPlatforms + 3 then
            if minPlatforms > 1 then
                minPlatforms -= 1
                diff += 1
                maxPlatforms = 17 - diff
            end if
        end if
    else
        if minPlatforms = 1 then
            if maxPlatforms > 1 then maxPlatforms -= 1
        end if
    end if
    
end sub

sub checkTech()
    
    if tech < 0 then tech = 0
    if tech > 100 then tech = 100
    if ((unlimit = 0 and tech >= tlev * 10) or (unlimit = 1 and tech >= totaltechs * 10)) and tech > 0 and ttree(tlev) = 0 then
        totaltechs += 1
        if unlimit = 1 then
            tech = 0
        else
            tech -= tlev * 10
        end if
        tblurb = tlev
        ttree(tlev) = 1
        upgrade(tlev).coll = 2
        tlev = 0
        for tt = 1 to 10
            if ttree(tt) = 0 and tlev = 0 then
                tlev = tt
            end if
        next tt
        fbs_Play_Sound hSound(14)
        ttime = 5000
        ud = 1
        upgd = 1
        tby = 340
    end if
    
end sub 

sub switchMode
    
    ssz = 1 - ssz
    if ssz = 0 then
        screenres sx, sy, depth, pages, GFX_WINDOWED or GFX_ALWAYS_ON_TOP
    else
        screenres sx, sy, depth, pages, GFX_FULLSCREEN or GFX_ALWAYS_ON_TOP
    end if
    
    'Disable Mouse Pointer
    setmouse ,,0
    
end sub
